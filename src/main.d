import std.stdio;
import std.getopt;

import binding;
import persistance;

void main(string[] args) {
    args = args[1..$];
    string in_filename = null;
    string out_filename = null;
    getopt(args,
        std.getopt.config.bundling,
        "i|input", &in_filename,
        "o|output", &out_filename
        );
    auto in_file = in_filename !is null ? File(in_filename, "rb") : stdin,
         out_file = out_filename !is null ? File(out_filename, "wb") : stdout;
    auto data = new Environment(args);
    out_file.write(data.toString());
}

//void open
