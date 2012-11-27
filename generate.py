"""
Generate images from latex data stored in ``data/*``.
"""

import os
import tempfile
import shutil
from glob import glob
import subprocess
import functools
import itertools


def memoize(func):
    cache = {}

    @functools.wraps(func)
    def wrapper(*args):
        try:
            return cache[args]
        except KeyError:
            result = cache[args] = func(*args)
            return result
    return wrapper


def mkdirp(p):
    """Do ``mkdir -p``"""
    if not os.path.isdir(p):
        os.makedirs(p)


def list_files(directory):
    for (dirpath, dirnames, filenames) in os.walk(directory):
        for d in dirnames:
            for f in list_files(os.path.join(dirpath, d)):
                yield f
        for f in filenames:
            yield os.path.join(dirpath, f)


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


def genelatex(strings, preamble=None):
    """
    Generate LaTeX document.

    Each string in `strings` will make a page.

    """
    yield r'\documentclass{article}'
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


@memoize
def load_preamble(directory):
    p = os.path.join(directory, '_preamble_.tex')
    if os.path.isfile(p):
        with open(p) as f:
            return f.read()
    parent = os.path.dirname(directory)
    if parent != directory:
        return load_preamble(parent)


def load_data(directory):
    for tex in filter(lambda x: x.endswith('.tex'), list_files(directory)):
        preamble = load_preamble(os.path.dirname(tex))
        noext = os.path.splitext(tex)[0]
        keyfile = '{0}.keywords'.format(noext)
        if os.path.basename(noext).startswith('_') and noext.endswith('_'):
            continue
        yield {
            'name': os.path.relpath(noext, directory),
            'tex': "${0}$".format(open(tex).read()),
            'keys': open(keyfile).read() if os.path.exists(keyfile) else '',
            'preamble': preamble,
        }


def generate_images(directory, resize=(50, 25)):
    """
    Generate mathematical symbol images from data in `directory`.
    """
    for (key, group) in itertools.groupby(load_data(directory),
                                          lambda x: x['preamble']):
        group = list(group)
        latex_to_pngs(
            texts=[d['tex'] for d in group],
            paths=[os.path.join('build', '{0}.png'.format(d['name']))
                   for d in group],
            resize=resize,
            preamble=group[0]['preamble'],
        )
        for d in group:
            with open(os.path.join('build',
                                   '{0}.keywords'.format(d['name'])),
                      'w') as f:
                f.write(d['keys'])


def main(args=None):
    from argparse import ArgumentParser
    parser = ArgumentParser(description=__doc__)
    parser.add_argument('directory', nargs='?', default='data')
    ns = parser.parse_args(args)
    return generate_images(**vars(ns))


if __name__ == '__main__':
    main()
