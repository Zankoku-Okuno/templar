import std.stdio;

import binding;
import persistance;

void main(string[] args) {
    auto data = new Environment(File(args[1]));
    write(data.toString());
}



