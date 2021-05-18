using System;
using System.IO;
using utils;

namespace gdml {
    class Parser {
        private GDMLDocument document;

        public Parser() {}
        public Parser(string path) {
            var res = this.Parse(path);
            if (res.Success)
                this.document = res.Data;
        }

        public Result<GDMLDocument> Parse(string path) {
            if (!File.Exists(path))
                return new ErrorResult<GDMLDocument>("File not found!");

            this.document = new GDMLDocument();

            foreach (var line in File.ReadAllLines(path)) {
                Console.WriteLine(line);
            }
            
            return new SuccessResult<GDMLDocument>(this.document);
        }
    }
}
