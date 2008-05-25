# from http://lukeplant.me.uk/blog.php?id=1107301665

import sys, os.path, codecs
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

default_enc = 'utf-8'

def pub(s):
    directives.register_directive('sc', code_block)
    overrides = {'input_encoding': default_enc
        , 'output_encoding': default_enc
        }
    return publish_parts(s, writer_name='html'
            , settings_overrides=overrides)

def tounicode(obj, encoding=default_enc):
    if isinstance(obj, basestring):
        if not isinstance(obj, unicode):
            obj = unicode(obj, encoding)
    return obj

def dictEncode(d):
    result = {}
    for k,v in d.iteritems():
        print type(v)
        result[k] = tounicode(v)
    return result

TEMPLATE = u'''
<?xml version="1.0" encoding="%(encoding)s" ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>
    <meta http-equiv="Content-Type" content="text/html; charset=%(encoding)s" />
    <title>%(title)s -- LIPL</title>
    <link href="style.css" rel="stylesheet" type="text/css" media="screen"/>
    <link href="print.css" rel="stylesheet" type="text/css" media="print"/>
</head>
<body>
<div id="wrap">
%(html_body)s
</div>
</body>
'''

def render_str(s, fn, dst):
    name = os.path.join(dst, fn)
    print "writing", name
    f = codecs.open(fn, encoding=default_enc)
    output = pub(s)
    output.setdefault('encoding', default_enc)
    outf = codecs.open(name, 'w', encoding=default_enc)
    outf.write(TEMPLATE  % output)
    outf.close()

def main(argv=None):
    '''argv[1] = file name to convert
argv[1] = destination to put the converted html (optional)'''
    if argv is None:
        argv = sys.argv
    if len(argv) < 2:
        print "Usage: %s file.lhs [dst-dir]" % argv[0]
        sys.exit(1)
    if len(argv) >= 3:
        dst = argv[2]
    else:
        dst = os.path.curdir

    fn = argv[1]
    bn = os.path.basename(fn)
    n,e = os.path.splitext(bn)
    name = os.path.join(dst, os.path.extsep.join([n, 'html']))
    print "writing", name
    f = codecs.open(fn, encoding=default_enc)
    new = []
    for l in f:
        if l[0] == u'>':
            new.append( u''.join([u'    >', l[1:]]) )
        else:
            new.append(l)
    f.close()


    output = pub(''.join(new))
    output.setdefault('encoding', default_enc)
    outf = codecs.open(name, 'w', encoding=default_enc)
    #output = dictEncode(output)
    outf.write(TEMPLATE

            % output)
    outf.close()

if __name__ == "__main__":
    main()
