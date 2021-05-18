using System;
using System.Collections.Generic;
using System.IO;
using utils;

namespace gdml {
    class Parser {
        private GDMLDocument document;
        private List<string> currentTag = new List<string>();
        private string rawData = null;
        private int currentPosition = 0;

        private class ReachedEndOfFile<T> : Result<T> {
            public ReachedEndOfFile() : base(default) {}
        }

        private string GetTypeFromTag(string tag) {
            var nSpace = tag.IndexOf(' ');
            var nColon = tag.IndexOf(':');

            if (nSpace == -1 && nColon == -1)
                return tag;
            
            if (nSpace == -1) return tag.Substring(0, nColon);
            if (nColon == -1) return tag.Substring(0, nSpace);

            if (nSpace > nColon)
                return tag.Substring(0, nColon);
            if (nColon > nSpace)
                return tag.Substring(0, nSpace);
            
            return "";
        }

        private Result<string> FindNextTag() {
            var startPoint = rawData.IndexOf('<', this.currentPosition);

            if (startPoint == -1)
                return new ReachedEndOfFile<string>();

            var endPoint = rawData.IndexOf('>', startPoint) + 1;

            this.currentPosition = endPoint;

            var ret = rawData.Substring(
                startPoint + 1,
                endPoint - 2 - startPoint
            ).Trim();

            if (ret.StartsWith('/'))
                currentTag.RemoveAt(currentTag.Count - 1);
            else if (!ret.EndsWith('/'))
                currentTag.Add(this.GetTypeFromTag(ret));
            
            Console.WriteLine('>' + string.Join(' ', this.currentTag));

            return new SuccessResult<string>(ret);
        }

        public Parser() {}
        public Parser(string path) {
            var res = this.Parse(path);
            if (res.Success)
                this.document = res.Data;
        }

        public Result<GDMLDocument> Parse(string path) {
            if (!File.Exists(path))
                return new ErrorResult<GDMLDocument>("File not found!");

            var data = File.ReadAllText(path).Replace('\n', ' ').Replace('\r', ' ');
            this.rawData = data;
            this.document = new GDMLDocument(data);
            
            while (!(this.FindNextTag() is ReachedEndOfFile<string>));
            
            return new SuccessResult<GDMLDocument>(this.document);
        }
    }
}
