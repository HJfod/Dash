using System;

namespace gdml {
    public static class Program {
        [STAThread]
        static void Main(string[] args) {
            if (args.Length == 0) {
                // todo: recurread directory for gdml files

                Console.WriteLine("no files provided");
                return;
            }

            var parser = new Parser();

            var res = parser.Parse(args[0]);

            if (res.Success)
                Console.WriteLine("Generated files in blah blah blah");
            else
                Console.WriteLine($"Error: {(res as utils.ErrorResult<GDMLDocument>).Message}");
        }
    }
}
