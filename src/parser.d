import std.stdio;
import std.string;

import binding;
import token;
import render;


/*
    parse data and templates (input + anything required by template inheritance)
    data parsing results in an Environment: wrap this low-level interface in a high-level, more template-sematic one
    each template parsing results in a token stream
    (each template extends zero or once. In any set of templates, exacly one inherits zero times, it is called the root template)
    render the token stream of the root template
        this only gets complex when rendering blocks, as blocks may be overridden, and thus need their own rendering
    if you see extends as the first token (non-first is error), then store the filename for later evaluation & continue parsing
        later, create a new parser with this as a parent, have it open&parse the extending file
    whenever blocks are found, they are registered in the parser outside the token stream
    if a block is found in a root template, then after registration, create a block-rendering token
    double-registration in a parser is an error
    when the root template is parsed, pass it to a renderer that has a connection to the environment

    at the end of parsing, we end up with a tree of ParseNodes.
        Each node has exactly one parent up the tree, except the root, which has no parent. The path through the tree following parent/child relationships is the trunk.
        Each node may have zero or more branches, which are also parseNode trees as described here. These branches are derived from %{include} directives
            branches are located within the token stream at a specific location.

    branch rendering:
        include:
            simply parse the included file as if it were the root of a tree
            if chain: eval each condition in order until one of them is true, then render the token stream associated with that cond
            loop:

    block rendering:
        if there is a %{more} token, then do ahead and render around it
            next, try to lookup the same block further up the trunk. when exists, render it
        else, try to lookup the same block further down the stack
            if it exists, then simply move on and render from there
            if not, then render and stop



*/



class TemplateParser {
private:
    Token[] stream = [];
    //stack of control structures being evaluated: I can add stuff here and modify the structure's state
    TokenStreamController[] control_stack = [];
    string buffer = "";
    //TODO traceback for diagnostics
public:
    TemplateParser parse(File file) {
        string line;
        while((line = file.readln()) !is null)
            this.parse(line);
        return this;
    }
    //WARNING this function only accounts for when chunks are line-by-line
    void parse(string chunk) {
        chunk = this.buffer ~ chunk; this.buffer = "";
        while(chunk.length > 0) {
            //TODO
            /*if eliminate_whitespace 
                if the whitespace mode flag is set, advance through whitespace. if there's still input, unset the flag
                if there's whitespace at the end-1, move it to the buffer*/
            ptrdiff_t index = 0;
           
            do { 
                /* look for the first control glyph, if there */ {
                    ptrdiff_t di = chunk[index..$].indexOf('{');
                    if (di < 1) break; // no control found
                    index += di;
                }
                char sigil = chunk[index-1];
                if (sigil == '%' || sigil == '!' || sigil == '$' || sigil == '@'
                ||  sigil == '?' || sigil == '&' || sigil == '#' || sigil == '\\')
                    this.emit(new StrToken(this.consume(index-1, chunk, index)));
                bool complete = true;
                switch (sigil) {
                    case '\\': complete = wrap_token(this.parse_escape(chunk, index)); break;
                    case '#':                        this.parse_comment(chunk, index); break;
                    case '$': assert(false); //STUB
                    case '@': assert(false); //STUB
                    case '%': assert(false); //STUB
                    case '?': assert(false); //STUB
                    case '!': assert(false); //STUB
                    case '&': assert(false); //STUB
                    default: ++index; break;
                }
                if (!complete) {
                    this.buffer = chunk;
                    chunk = ""; index = 0;
                }
            } while (true);
            if (chunk.length > 0) { this.emit(new StrToken(chunk)); chunk = ""; }
        }
    }

    TemplateRenderer renderer(Environment env) {
        //TODO unbuffer
        return new TemplateRenderer(env, this.stream);
    }

private:
    void emit(Token token) {
        //TODO if the token would modify control stack, then handle that here
        if (this.control_stack.length == 0) this.stream ~= token;
        else this.control_stack[$-1].emit(token);
    }

    bool wrap_token(Token token) {
        if (token !is null) this.emit(token);
        return token !is null;
    }

    Token parse_escape(ref string chunk, ref ptrdiff_t index) {
        ptrdiff_t end_index = -1;
        if ((end_index = chunk.indexOf('}')) == -1) return null; // undefined behavoir
        return new StrToken(this.consume(end_index+1, chunk, index)[2..$-1] ~ "{");
    }//TESTME

    void parse_comment(ref string chunk, ref ptrdiff_t index) {
        chunk = chunk[2..$];
        ptrdiff_t end_index;
        while((end_index = chunk.indexOf('}')) != -1) {
            if (end_index+1 < chunk.length && chunk[end_index+1] == '#') {
                this.consume(end_index+2, chunk, index);
                return;
            }
            else
                chunk = chunk[end_index+1..$];
        }
        chunk = ""; index = 0;
        this.buffer = "#{";
    }//TESTME

     string consume(ptrdiff_t n, ref string chunk, ref ptrdiff_t index) {
        string consumed = chunk[0..n];
        chunk = chunk[n..$];
        index = 0;
        return consumed;
    }//TESTME
}

private:

abstract class Renderable {
public:
    abstract string render();
    void render(File file) { file.write(this.render()); }
}


abstract class TokenStreamController : Renderable {
private:
    TokenStreamController outer;
public:
    abstract void emit(Token token);
}

class LoopController : TokenStreamController {
    Token[] body_tokens;
    Token[] empty_tokens;
}


