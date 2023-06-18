use arcstr::ArcStr;
use indexmap::IndexMap;
use std::path::{Path, PathBuf};

use super::error::IncludeError;
use crate::SourceKind;

type Files = codespan::Files<ArcStr>;

pub struct FileManager {
    /// The directories to consider as part of the system search path.
    pub search_path: Vec<PathBuf>,
    files: IndexMap<PathBuf, (ArcStr, SourceKind)>,
    pub reporting: Files,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum IncludeKind {
    Local,
    System,
    Next,
}

macro_rules! built_in_headers {
    ( $($filename: literal),+ $(,)? ) => {
        [
            // Relative to the current file, not the crate root
            $( ($filename, include_str!(concat!("headers/", $filename))) ),+
        ]
    };
}

const PRECOMPILED_HEADERS: [(&str, &str); 2] = built_in_headers! {
    "stdarg.h",
    "stddef.h",
};

fn get_builtin_header(expected: impl AsRef<str>) -> Option<&'static str> {
    PRECOMPILED_HEADERS
        .iter()
        .find(|&(path, _)| path == &expected.as_ref())
        .map(|x| x.1)
}

impl FileManager {
    pub fn new() -> Self {
        let system_path = "x86_64-linux-gnu";
        Self {
            search_path: vec![
                PathBuf::from(format!("/usr/local/include/{}", system_path)).into(),
                Path::new("/usr/local/include").into(),
                PathBuf::from(format!("/usr/include/{}", system_path)).into(),
                Path::new("/usr/include").into(),
            ],
            files: IndexMap::new(),
            reporting: Files::new(),
        }
    }
    fn find_include_path(
        &mut self,
        filename: &Path,
        current_path: &Path,
        kind: IncludeKind,
    ) -> Result<PathBuf, IncludeError> {
        if filename.as_os_str().is_empty() {
            return Err(IncludeError::EmptyInclude);
        }

        let not_found = |this: &Self, filename: &Path| {
            Err(IncludeError::FileNotFound(
                filename.to_string_lossy().to_string(),
            ))
        };

        let mut can_include_match = kind != IncludeKind::Next;

        // absolute path, ignore everything except the filename
        // e.g `#include </usr/local/include/stdio.h>`
        if filename.is_absolute() {
            return if filename.exists() {
                Ok(filename.to_owned())
            } else {
                not_found(self, filename)
            };
        }
        // local include: #include "dict.h"
        if kind != IncludeKind::System {
            let relative_path = &current_path
                .parent()
                .unwrap_or_else(|| std::path::Path::new(""));
            let resolved = relative_path.join(filename);
            if resolved.exists() {
                if can_include_match {
                    return Ok(resolved);
                } else {
                    can_include_match = true;
                }
            }
        }
        // if we don't find it locally, we fall back to system headers
        // this is part of the spec! http://port70.net/~nsz/c/c11/n1570.html#6.10.2p3
        for path in &self.search_path {
            let mut buf = path.clone();
            buf.push(filename);
            if buf.exists() {
                if can_include_match {
                    return Ok(buf);
                } else {
                    can_include_match = true;
                }
            }
        }

        not_found(self, filename)
    }

    // we've done the parsing for an `#include`,
    // now we want to do the dirty work of reading it into memory
    pub fn include_path(
        &mut self,
        filename: &Path,
        current_path: &Path,
        kind: IncludeKind,
    ) -> Result<(ArcStr, SourceKind), IncludeError> {
        let (src, kind) = match self.find_include_path(&filename, current_path, kind) {
            Ok(path) => {
                if let Some(data) = self.files.get(&path) {
                    data.clone()
                } else {
                    let src: ArcStr = std::fs::read_to_string(&path)
                        .map_err(|err| IncludeError::IO(err.to_string()))?.into();

                    let id = self.reporting.add(path.clone(), src.clone());
                    let entry = self.files.entry(path);

                    entry.or_insert((src, SourceKind::File(id))).clone()
                }
            }
            Err(not_found) => {
                let filename = match filename.file_name().and_then(|f| f.to_str()) {
                    None => return Err(not_found),
                    Some(f) => f,
                };

                match get_builtin_header(filename) {
                    Some(file) => {
                        let mut path = PathBuf::from("<builtin>");
                        path.push(filename);
                        (ArcStr::from(file), SourceKind::Generated)
                    }
                    None => return Err(not_found),
                }
            }
        };

        Ok((src, kind))
    }
}
