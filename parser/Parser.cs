using System;
using System.Collections;

namespace gdml {
    class GDMLNode {
        private ArrayList children;
        private string name;
        public string content {
            get { return content; }
            set { content = value; }
        };

        public void AddChild(GDMLNode node) {
            this.children.add(node);
        }
    }
    
    class GDMLDocument : GDMLNode {

    }

    class Parser {
        public Parser() {
            Console.WriteLine("bruh");
        }
    }
}
