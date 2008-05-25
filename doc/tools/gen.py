'''generate documents'''

import sys, os.path, os, shutil
import lhs2html

#proj/doc/tools
base_dir = os.path.dirname(__file__)

# proj/src
src_dir = os.path.abspath(os.path.join(base_dir, os.pardir, os.pardir, 'src'))

# proj/doc
dst_dir = os.path.abspath(os.path.join(base_dir, os.pardir))
doc_dir = dst_dir

def want_to(f):
    name, ext = os.path.splitext(f)
    ext = ext.lower()
    if ext == '.lhs' and name[0].isupper():
        return True
    return False


def main(argv=None):
    '''argv[1] = src_dir (where lhs are located. optional)
argv[2] = dst_dir (where to put generated htmls. optional)'''

    global src_dir, dst_dir, doc_dir

    if argv is None:
        argv = sys.argv
    if len(argv) >= 2:
        src_dir = argv[1]
    if len(argv) >= 3:
        dst_dir = argv[2]

    print 'src_dir', src_dir
    print 'dst_dir', dst_dir
    tree = os.walk(src_dir)
    for root, dirs, files in tree:
        for x in files:
            if want_to(x):
                f = os.path.join(root, x)
                lhs2html.main(['', f, dst_dir])

    files = ['index.txt', 'langref.txt', 'tutorial.txt']
    for x in files:
        f = os.path.join(doc_dir, x)
        lhs2html.main(['', f, dst_dir])

    style = os.path.abspath(os.path.join(doc_dir, 'tools', 'style.css'))
    print_style = os.path.abspath(os.path.join(doc_dir, 'tools', 'print.css'))
    print 'copying', style
    shutil.copy(style, doc_dir)
    print 'copying', print_style
    shutil.copy(print_style, doc_dir)

if __name__ == "__main__":
    main()
