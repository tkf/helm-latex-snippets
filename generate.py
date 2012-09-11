import os
import tempfile
import shutil
from glob import glob
import subprocess


def mkdirp(p):
    if not os.path.isdir(p):
        os.makedirs(p)


def latex_to_pngs(texts, paths, **kwds):
    try:
        workdir = tempfile.mkdtemp()
        tmpfile = os.path.join(workdir, "tmp.tex")
        dvifile = os.path.join(workdir, "tmp.dvi")
        outfile = os.path.join(workdir, "tmp-%09d.png")

        with open(tmpfile, "w") as f:
            f.writelines(genelatex(texts, **kwds))

        with open(os.devnull, 'w') as devnull:
            subprocess.check_call(
                ["latex", "-halt-on-error", tmpfile], cwd=workdir,
                stdout=devnull, stderr=devnull)

            subprocess.check_call(
                ["dvipng", "-T", "tight", "-x", "1500", "-z", "9",
                 "-bg", "transparent", "-o", outfile, dvifile], cwd=workdir,
                stdout=devnull, stderr=devnull)

        outlist = sorted(glob(os.path.join(workdir, "tmp-*.png")))
        for (src, dest) in zip(outlist, paths):
            mkdirp(os.path.dirname(dest))
            shutil.move(src, dest)
    finally:
        shutil.rmtree(workdir)


def genelatex(strings, packages=[], preamble=None):
    """Generate LaTeX document."""
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
    with open(os.path.join("data", "math", name)) as f:
        return map(str.strip, f.readlines())


def generate_image_math():
    package = 'latex2e'
    names = load_data_math(package)
    texts = map("$\\{0}$".format, names)
    paths = [os.path.join("build", "math", package, "{0}.png".format(n))
             for n in names]
    latex_to_pngs(texts, paths)


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
    parser_math

    def applyargs(func, **kwds):
        return func(**kwds)

    ns = parser.parse_args(args)
    return applyargs(**vars(ns))


if __name__ == '__main__':
    main()
