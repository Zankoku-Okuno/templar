

/*
say I've got a big string:
    find first of \s*[$@?%!\\]\{
    emit token uptil that point and advance through whitespace
    now, switch modes depending on the new start of string
        probably that means extracting using a regex, then emitting token

now I've got a list of tokens (type+data) that I can organize into a tree and render
*/
