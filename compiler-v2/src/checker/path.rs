
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Ident {
    name: String,
    decorator: bool,
}

impl From<&str> for Ident {
    fn from(value: &str) -> Self {
        Self::from(value.to_string())
    }
}

impl From<String> for Ident {
    fn from(value: String) -> Self {
        if let Some(name) = value.strip_prefix('@') {
            Self { name: name.to_string(), decorator: true }
        }
        else {
            Self { name: value, decorator: false }
        }
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", if self.decorator { "@" } else { "" }, self.name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct IdentPath {
    components: Vec<Ident>,
    absolute: bool,
}

impl IdentPath {
    pub fn new<T: Into<Vec<Ident>>>(path: T, absolute: bool) -> Self {
        Self { components: path.into(), absolute }
    }
    pub fn parse(mut value: &str) -> Self {
        let absolute;
        if let Some(relative) = value.strip_prefix("::") {
            absolute = true;
            value = relative;
        }
        else {
            absolute = false;
        }
        Self {
            components: value.split("::").map(|v| Ident::from(v)).collect(),
            absolute
        }
    }
    pub fn to_full(&self) -> FullIdentPath {
        FullIdentPath::new(self.components.clone())
    }
    pub fn is_absolute(&self) -> bool {
        self.absolute
    }
}

impl Display for IdentPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", 
            if self.absolute { "::" } else { "" },
            self.components.iter().map(|c| c.to_string()).collect::<Vec<_>>().join("::")
        )
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub(crate) struct FullIdentPath {
    components: Vec<Ident>,
}

impl FullIdentPath {
    pub fn new<T: Into<Vec<Ident>>>(path: T) -> Self {
        Self { components: path.into() }
    }
    pub fn ends_with(&self, path: &IdentPath) -> bool {
        self.to_string().ends_with(&path.to_string())
    }
    pub fn push(&mut self, ident: Ident) {
        self.components.push(ident);
    }
    pub fn pop(&mut self) {
        self.components.pop();
    }
    // Add another path on the end of this full path. If the other path is 
    // absolute, returns it as a full path
    pub fn join(&self, path: &IdentPath) -> FullIdentPath {
        if path.absolute {
            path.to_full()
        }
        else {
            let mut new = self.clone();
            new.components.extend(path.components.iter().cloned());
            new
        }
    }
    pub fn is_empty(&self) -> bool {
        self.components.is_empty()
    }
}

impl Display for FullIdentPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "::{}", self.components.iter().map(|c| c.to_string()).collect::<Vec<_>>().join("::"))
    }
}
