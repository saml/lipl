#!/usr/bin/python

"""
gendoc.py
from http://lukeplant.me.uk/blog.php?id=1107301665

A minimal front end to the Docutils Publisher, producing HTML,
with an extension for colouring code-blocks
"""

#try:
#    import locale
#    locale.setlocale(locale.LC_ALL, '')
#except:
#    pass


from docutils import nodes, parsers
from docutils.parsers.rst import states, directives
from docutils.core import publish_cmdline, default_description
from pah import Writer
import tempfile, os

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

def main(argv=None):
    # Register
    directives.register_directive( 'sc', code_block )


    description = ('Generates (X)HTML documents from standalone reStructuredText '
                   'sources.  ' + default_description)

    pah_writer = Writer()
    # Command line
    publish_cmdline(writer=pah_writer, description=description)

if __name__ == "__main__":
    main()

