using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using utils;

namespace gdml {
    class Parser {
        private GDMLDocument Document;
        private List<GDMLNode> CurrentTag = new List<GDMLNode>();
        private string RawData = null;
        private int CurrentPosition = 0;

        private class ReachedEndOfFile<T> : Result<T> {
            public ReachedEndOfFile() : base(default) {}
        }

        private Result<string> FindNextTag() {
            var startPoint = RawData.IndexOf('<', this.CurrentPosition);

            if (startPoint == -1)
                return new ReachedEndOfFile<string>();

            var endPoint = RawData.IndexOf('>', startPoint) + 1;

            this.CurrentPosition = endPoint;

            var ret = RawData.Substring(
                startPoint + 1,
                endPoint - 2 - startPoint
            ).Trim();

            if (ret.StartsWith('/'))
                CurrentTag.RemoveAt(CurrentTag.Count - 1);
            else {
                var res = new GDMLNode();
                var success = res.Parse(ret);

                if (success.Failure)
                    return success;

                if (this.CurrentTag.Count == 0)
                    this.Document.Root = res;
                else
                    CurrentTag.Last().AddChild(res);

                if (!ret.EndsWith('/'))
                    CurrentTag.Add(res);
            }
            
            Console.WriteLine(string.Join(' ', this.CurrentTag));

            return new SuccessResult<string>(ret);
        }

        public Parser() {}
        public Parser(string path) {
            var res = this.Parse(path);
            if (res.Success)
                this.Document = res.Data;
        }

        public Result<GDMLDocument> Parse(string path) {
            if (!File.Exists(path))
                return new ErrorResult<GDMLDocument>("File not found!");

            var data = File.ReadAllText(path).Replace('\n', ' ').Replace('\r', ' ');
            this.RawData = data;
            this.Document = new GDMLDocument(data);
            
            while (!(this.FindNextTag() is ReachedEndOfFile<string>));
            
            return new SuccessResult<GDMLDocument>(this.Document);
        }
    }
}
