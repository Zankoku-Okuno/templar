import std.stdio;
import std.array;
import std.string;
import dx.string;

import binding;

abstract class Token {
    abstract string render(SemanticEnvironment env);
    void render(SemanticEnvironment env, File file) { file.write(this.render(env)); }
}

class StrToken : Token {
private:
    string data;
public:
    this(string data) { this.data = data; }
    string render(SemanticEnvironment env) { return this.data; }
}

class ScalarToken : Token {
private:
    string[] lookup;
public:
    this(string lookup) {
        lookup = lookup.strip();
        if (lookup[0] == '.' || lookup[$-1] == '.' || lookup.contains(".."))
            throw new Exception("Empty variable lookup path segment."); //TODO better diagnostics
        this.lookup = lookup.split(".");
        foreach(size_t i, string elem; this.lookup)
            this.lookup[i] = elem.strip();       
    }//TESTME

    string render(SemanticEnvironment env) {
        return env.resolve_scalar(this.lookup);
    }
}
