import sys, os.path
from docutils import nodes, parsers
from docutils.parsers.rst import directives
from docutils.core import publish_parts

def get_highlighter(language):

    from pygments import lexers, util, highlight, formatters
    import StringIO

    try:
        lexer = lexers.get_lexer_by_name(language)
    except util.ClassNotFound:
        return None

    formatter = formatters.get_formatter_by_name('html')
    def _highlighter(code):
        outfile = StringIO.StringIO()
        parsed = highlight(code, lexer, formatter, outfile)
        return outfile.getvalue()
    return _highlighter

# Docutils directives:
def code_block(name, arguments, options, content, lineno,
               content_offset, block_text, state, state_machine):
    """
    The code-block directive provides syntax highlighting for blocks
    of code.  It is used with the the following syntax::

    .. code-block:: python

       import sys
       def main():
           sys.stdout.write("Hello world")

    """
    language = arguments[0]
    highlighter = get_highlighter(language)
    if highlighter is None:
        error = state_machine.reporter.error(
            'The "%s" directive does not support language "%s".' % (name, language),
            nodes.literal_block(block_text, block_text), line=lineno)

    if not content:
        error = state_machine.reporter.error(
            'The "%s" block is empty; content required.' % (name),
            nodes.literal_block(block_text, block_text), line=lineno)
        return [error]

    include_text = highlighter("\n".join(content))
    html = '<div class="syntax %s">\n%s\n</div>\n' % (language, include_text)
    raw = nodes.raw('',html, format='html')
    return [raw]

code_block.arguments = (1,0,0)
code_block.options = {'language' : parsers.rst.directives.unchanged }
code_block.content = 1

def pub(s):
    directives.register_directive('sc', code_block)
    overrides = {'input_encoding': 'unicode'
        , 'output_encoding': 'unicode'
        , }
    return publish_parts(s, writer_name='html')

def main(argv=None):
    argv = sys.argv
    fn = argv[1]
    bn = os.path.basename(fn)
    n,e = os.path.splitext(bn)
    name = os.path.extsep.join([n, 'html'])
    f = open(fn)
    new = []
    for l in f:
        if l[0] == '>':
            new.append( ''.join(['    >', l[1:]]) )
        else:
            new.append(l)
    f.close()

    template = '''
<?xml version="1.0" encoding="%(encoding)s" ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>
    <meta http-equiv="Content-Type" content="text/html; charset=%(encoding)s" />
    <title>%(title)s -- LIPL</title>
    <link href="style.css" rel="stylesheet" type="text/css" media="all"/>
</head>
<body>
%(html_body)s
</body>
'''
    output = pub(''.join(new))
    outf = open(name, 'w')
    outf.write(template % output)
    outf.close()

if __name__ == "__main__":
    main()
