"""
HTML Writer.
"""

__docformat__ = 'reStructuredText'


import sys
import os
import os.path
import docutils
from docutils import frontend, nodes, utils, writers
from docutils.writers import html4css1


class Writer(html4css1.Writer):

    default_stylesheet = 'style.css'

    default_stylesheet_path = utils.relative_path(
        os.path.join(os.getcwd(), 'dummy'),
        os.path.join(os.path.dirname(__file__), default_stylesheet))

    default_template = 'template.html'

    default_template_path = utils.relative_path(
        os.path.join(os.getcwd(), 'dummy'),
        os.path.join(os.path.dirname(__file__), default_template))

    settings_spec = html4css1.Writer.settings_spec + (
        'PEP/HTML-Specific Options',
        'The default value for the --stylesheet-path option (defined in '
        'HTML-Specific Options above) is "%s" for the PEP/HTML writer.'
        % default_stylesheet_path,
        (('Specify a template file.  Default is "%s".' % default_template_path,
          ['--tem'],
          {'default': default_template_path, 'metavar': '<file>'}),
         ('Python\'s home URL.  Default is "http://www.python.org".',
          ['--python-home'],
          {'default': 'http://www.python.org', 'metavar': '<URL>'}),
         ('Home URL prefix for PEPs.  Default is "." (current directory).',
          ['--pep-home'],
          {'default': '.', 'metavar': '<URL>'}),
         # For testing.
         (frontend.SUPPRESS_HELP,
          ['--no-random'],
          {'action': 'store_true', 'validator': frontend.validate_boolean}),))

    settings_default_overrides = {'stylesheet_path': default_stylesheet_path}

    relative_path_settings = (html4css1.Writer.relative_path_settings
                              + ('tem',))

    #config_section = 'pep_html writer'
    #config_section_dependencies = ('writers', 'html4css1 writer')

    def __init__(self):
        html4css1.Writer.__init__(self)
        self.translator_class = HTMLTranslator

    def translate(self):
        html4css1.Writer.translate(self)
        settings = self.document.settings
        template = open(settings.tem).read()
        # Substitutions dict for template:
        subs = {}
        subs['encoding'] = settings.output_encoding
        subs['version'] = docutils.__version__
        #subs['head_prefix'] = ''
        #subs['head'] = ''
        #subs['body_prefix'] = ''
        #subs['body_pre_docinfo'] = ''
        #subs['docinfo'] = ''
        subs['stylesheet'] = ''.join(self.stylesheet)
        #index = self.document.first_child_matching_class(nodes.field_list)
        #header = self.document[index]
        subs['title'] = self.document[0].astext()
        #subs['navigation'] = "hello, this is navigation"
        subs['body'] = ''.join(
            self.body_pre_docinfo + self.docinfo + self.body)
        subs['body_suffix'] = ''.join(self.body_suffix)
        self.output = template % subs


class HTMLTranslator(html4css1.HTMLTranslator):

    def depart_field_list(self, node):
        html4css1.HTMLTranslator.depart_field_list(self, node)
        if 'rfc2822' in node['classes']:
             self.body.append('<hr />\n')
