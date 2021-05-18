using System.Collections.Generic;

namespace gdml {
    abstract class PGDMLAttribute {
        public string name { get; internal set; }
    }

    class GDMLAttribute<T> : PGDMLAttribute {
        public T value { get; internal set; }

        public GDMLAttribute(string name, T val) {
            this.name = name;
            this.value = val;
        }
    }

    class GDMLNode {
        public List<GDMLNode> children { get; }
        public List<PGDMLAttribute> attributes { get; }
        public string type { get; internal set; }
        public string name;
        public string content;

        public void AddChild(GDMLNode node) {
            this.children.Add(node);
        }

        public void AddAttribute(PGDMLAttribute attr) {
            this.attributes.Add(attr);
        }

        public void AddAttribute<T>(string name, T value) {
            this.attributes.Add(new GDMLAttribute<T>(name, value));
        }
    }

    class GDMLScene : GDMLNode {
        public GDMLScene() {
            this.type = "scene";
        }
    }
    
    class GDMLDocument {
        public string rawData { get; internal set; }
        public List<GDMLScene> scenes { get; internal set; }

        public GDMLDocument(string raw) {
            this.rawData = raw;
        }
    }
}