"""
Generate images from latex data stored in ``data/*``.
"""

import os
import tempfile
import shutil
from glob import glob
import subprocess


def mkdirp(p):
    """Do ``mkdir -p``"""
    if not os.path.isdir(p):
        os.makedirs(p)


def check_call_error_message(cmd, *args, **kwds):
    proc = subprocess.Popen(
        cmd, *args, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, **kwds)
    (stdout, _) = proc.communicate()
    retcode = proc.returncode
    if retcode != 0:
        print "*" * 50
        print stdout,
        print "*" * 50
        raise subprocess.CalledProcessError(retcode, cmd)


def latex_to_pngs(texts, paths, resize=None, **kwds):
    """
    Generate images from latex `texts`.

    For each latex text in `texts` generate an image and copy it
    to the path (of the same index) in `paths`.

    """
    assert len(texts) == len(paths)
    try:
        workdir = tempfile.mkdtemp()
        tmpfile = os.path.join(workdir, "tmp.tex")
        dvifile = os.path.join(workdir, "tmp.dvi")
        outfile = os.path.join(workdir, "tmp-%09d.png")

        with open(tmpfile, "w") as f:
            f.writelines(genelatex(texts, **kwds))

        check_call_error_message(
            ["latex", "-halt-on-error", tmpfile],
            cwd=workdir)

        check_call_error_message(
            ["dvipng", "-T", "tight", "-x", "1500", "-z", "9",
             "-bg", "transparent", "-o", outfile, dvifile],
            cwd=workdir)

        outlist = sorted(glob(os.path.join(workdir, "tmp-*.png")))
        assert len(outlist) == len(paths)
        map(mkdirp, set(map(os.path.dirname, paths)))

        if resize:
            for (src, dest) in zip(outlist, paths):
                check_call_error_message(
                    ['convert', src,
                     '-size', '{0}x{1}'.format(*resize),
                     'xc:white', '+swap', '-gravity', 'center',
                     '-composite', dest])
        else:
            for (src, dest) in zip(outlist, paths):
                shutil.move(src, dest)
    finally:
        shutil.rmtree(workdir)


def genelatex(strings, packages=[], preamble=None):
    """
    Generate LaTeX document.

    Each string in `strings` will make a page.

    """
    yield r'\documentclass{article}'
    for pack in packages:
        yield r'\usepackage{{{0}}}'.format(pack)
    yield r'\pagestyle{empty}'
    if preamble:
        yield preamble
    yield r'\begin{document}'
    for s in strings:
        yield s
        yield "\n"
        yield r"\newpage"
        yield "\n"
    yield r'\end{document}'


def load_data_math(name):
    """Load data from ``data/math/{name}``."""
    with open(os.path.join("data", "math", name)) as f:
        return map(str.strip, f.readlines())


def generate_image_math(packages):
    """
    Generate mathematical symbol images.

    Generated images will be in ``build/math/{package}/``.

    """
    if not packages:
        packages = filter(lambda x: not x.endswith('~'),
                          os.listdir(os.path.join('data', 'math')))
    map(generate_image_math_package, packages)


def generate_image_math_package(package):
    basepath = os.path.join("build", "math", package)
    if os.path.isdir(basepath):
        shutil.rmtree(basepath)
    names = load_data_math(package)
    texts = map("$\\{0}$".format, names)
    paths = [os.path.join(basepath, "{0}.png".format(n)) for n in names]
    latex_to_pngs(texts, paths, (50, 25),
                  packages=[package] if package != 'latex2e' else [])


def main(args=None):
    from argparse import ArgumentParser
    parser = ArgumentParser(description=__doc__)
    subparsers = parser.add_subparsers()

    def make_subparser(cmd, func):
        subparser = subparsers.add_parser(cmd, help=func.__doc__)
        subparser.set_defaults(func=func)
        return subparser

    # math
    parser_math = make_subparser('math', generate_image_math)
    parser_math.add_argument(
        '--package', dest='packages', default=[], action='append',
        help='Generate images for data stored in data/math/PACKAGE')

    def applyargs(func, **kwds):
        return func(**kwds)

    ns = parser.parse_args(args)
    return applyargs(**vars(ns))


if __name__ == '__main__':
    main()
