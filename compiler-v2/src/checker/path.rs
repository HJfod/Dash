
use std::fmt::Display;

#[derive(Debug)]
struct IdentPath {
    components: Vec<String>,
    absolute: bool,
}

impl IdentPath {
    pub fn new<T: Into<Vec<String>>>(path: T, absolute: bool) -> Self {
        Self { components: path.into(), absolute }
    }

    pub fn into_full(self) -> FullIdentPath {
        FullIdentPath::new(self.components)
    }
}

impl Display for IdentPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}{}", 
            if self.absolute { "::" } else { "" },
            self.components.join("::")
        ))
    }
}

#[derive(Debug)]
struct FullIdentPath {
    components: Vec<String>,
}

impl FullIdentPath {
    pub fn new<T: Into<Vec<String>>>(path: T) -> Self {
        Self { components: path.into() }
    }

    pub fn ends_with(&self, path: &IdentPath) -> bool {
        self.to_string().ends_with(&path.to_string())
    }
}

impl Display for FullIdentPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("::{}", self.components.join("::")))
    }
}
