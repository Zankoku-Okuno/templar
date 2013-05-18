import std.stdio;

abstract class Token {
    abstract string render();
    void render(File file) { file.write(this.render()); }
}

class StrToken : Token {
private:
    string data;
public:
    this(string data) { this.data = data; }
    string render() { return this.data; }
}
