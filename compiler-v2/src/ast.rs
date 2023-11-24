
use std::collections::HashMap;

pub enum Child {
    Node(Node),
    Maybe(Option<Node>),
    List(Vec<Node>),
}

pub struct Node {
    children: HashMap<String, Child>,
}

impl Node {
    pub fn new(children: HashMap<String, Child>) -> Self {
        Self { children }
    }
}
