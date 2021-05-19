using System.Collections.Generic;
using utils;

namespace gdml {
    abstract class PGDMLAttribute {
        public string Name { get; internal set; }

        public abstract string ValueToString();
    }

    class GDMLAttribute<T> : PGDMLAttribute {
        public T Value { get; internal set; }

        public override string ValueToString() {
            return Value.ToString();
        }

        public GDMLAttribute(string name, T val) {
            this.Name = name;
            this.Value = val;
        }
    }

    class GDMLNode {
        public List<GDMLNode> Children { get; } = new List<GDMLNode>();
        public List<PGDMLAttribute> Attributes { get; } = new List<PGDMLAttribute>();
        public string Type { get; internal set; }
        public string Name;
        public string Content;

        public void AddChild(GDMLNode node) {
            this.Children.Add(node);
        }

        public void AddAttribute(PGDMLAttribute attr) {
            this.Attributes.Add(attr);
        }

        public void AddAttribute<T>(string name, T val) {
            this.Attributes.Add(new GDMLAttribute<T>(name, val));
        }

        public void AddAttributeFT(string name, string val) {
            this.Attributes.Add(new GDMLAttribute<string>(name, val));
        }

        public Result<string> Parse(string data) {
            data = data.Trim();
            var iType = data.IndexOfAny(new char[] { ' ', ':' });

            if (iType == -1)
                this.Type = data;
            else {
                this.Type = data.Substring(0, iType);
                var iAttrs = iType + 1;

                if (data[iType] == ':') {
                    var iName = data.IndexOf(' ', iType);

                    if (iName == -1) {
                        data.Substring(iType + 1);

                        return new SuccessResult<string>(data);
                     } else
                        data.Substring(iType + 1, iName - iType - 1);
                    
                    iAttrs = iName + 1;
                }

                var attrStr = data.Substring(iAttrs);

                string cName = "";
                string collect = "";
                bool insideQuote = false;
                bool assign = false;

                foreach (char c in attrStr) {
                    if (insideQuote)
                        if (c == '\"') {
                            insideQuote = false;

                            if (assign) {
                                this.AddAttributeFT(cName, collect);
                                cName = "";
                                assign = false;
                            } else {
                                if (cName != "")
                                    this.AddAttribute<bool>(cName, true);
                                cName = collect;
                            }
                            
                            collect = "";
                        } else
                            collect += c;
                    else
                        if (c == '\"')
                            insideQuote = true;
                        else
                            if (c == '=') {
                                cName = collect;
                                assign = true;
                            } else
                                if (c == ' ') {
                                    if (assign) {
                                        this.AddAttributeFT(cName, collect);
                                        cName = "";
                                        assign = false;
                                    } else {
                                        if (cName != "")
                                            this.AddAttribute<bool>(cName, true);
                                        cName = collect;
                                    }
                                } else
                                    collect += c;
                }
            }

            return new SuccessResult<string>(data);
        }

        public override string ToString() {
            string attrs = "";

            foreach (var attr in this.Attributes)
                attrs += $"{attr.Name}: {attr.ValueToString()}, ";

            return $"<{this.Type}:${this.Name}> [ {attrs}]";
        }

        public GDMLNode () {}
        public GDMLNode (string data) { this.Parse(data); }
    }

    class GDMLScene : GDMLNode {
        public GDMLScene() {
            this.Type = "scene";
        }
    }
    
    class GDMLDocument {
        public string RawData { get; internal set; }
        public List<GDMLScene> Scenes { get; internal set; }
        public GDMLNode Root { get; set; }

        static public GDMLNode CreateNode(string type) => type switch {
            "scene" => new GDMLScene(),
            _ => new GDMLNode(),
        };

        public GDMLDocument(string raw) {
            this.RawData = raw;
        }
    }
}