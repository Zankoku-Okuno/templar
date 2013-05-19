import std.stdio;

import binding;
import token;

class TemplateRenderer {
private:
    SemanticEnvironment env;
    Token[] stream;
public:
    this(Environment env, Token[] stream) {
        this.env = new SemanticEnvironment(env);
        this.stream = stream;
    }

    void render(File file) {
        foreach(Token token; this.stream)
            token.render(this.env, file);
    }
}

