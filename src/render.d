import std.stdio;

import binding;
import token;

class TemplateRenderer {
private:
    Environment env;
    Token[] stream;
public:
    this(Environment env, Token[] stream) {
        this.env = env;
        this.stream = stream;
    }

    void render(File file) {
        foreach(Token token; this.stream)
            token.render(file);
    }
}

//TODO
class EnvInterface {

}
