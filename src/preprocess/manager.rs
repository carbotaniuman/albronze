use arcstr::ArcStr;
use indexmap::IndexMap;

use std::path::{Path, PathBuf};

use super::error::IncludeError;

pub struct FileManager {
    /// The directories to consider as part of the system search path.
    pub search_path: Vec<PathBuf>,
    files: IndexMap<PathBuf, ArcStr>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum IncludeKind {
    Local,
    System,
    Next,
}

pub struct FileId(usize);

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
    ) -> Result<(ArcStr, FileId), IncludeError> {
        let (src, id) = match self.find_include_path(&filename, current_path, kind) {
            Ok(path) => {
                if let Some((id, _, src)) = self.files.get_full(&path) {
                    (src.clone(), id)
                } else {
                    let src = std::fs::read_to_string(&path)
                        .map_err(|err| IncludeError::IO(err.to_string()))?;

                    let entry = self.files.entry(path);
                    let saved_index = entry.index();

                    (entry.or_insert(ArcStr::from(src)).clone(), saved_index)
                }
            }
            Err(not_found) => {
                let filename = match filename.file_name().and_then(|f| f.to_str()) {
                    None => return Err(not_found),
                    Some(f) => f,
                };

                // TODO: wire in builtins
                // match get_builtin_header(filename) {
                //     Some(file) => {
                //         let mut path = PathBuf::from("<builtin>");
                //         path.push(filename);
                //         (path, ArcStr::from(file))
                //     }
                //     None => return Err(not_found),
                // }
                return Err(not_found);
            }
        };

        Ok((src, FileId(id)))
    }
}
