#from setuptools import setup
from distutils.core import setup

setup(
    name = 'PScheme',
    version = '0.1',
    description = 'Scheme Interpreter',
    long_description=open("README.rst", 'r').read(),
    author = "Andrzej Radecki",
    author_email = "ndrwrdck@gmail.com",
    url = "http://github.com/andrzej-r/PScheme",
    license = "BSD",
    keywords = "scheme r5rs repl restricted embedded interpreter sandbox",
    classifiers = [
        "Programming Language :: Python",
        "Development Status :: 2 - Pre-Alpha",
        "License :: OSI Approved :: BSD License",
        "Intended Audience :: Developers",
        "Operating System :: OS Independent",
        "Topic :: Software Development :: Interpreters",
        "Topic :: System :: Software Distribution",
        "Topic :: Security",
        "Programming Language :: Python",
        "Programming Language :: Scheme",
        "Programming Language :: Lisp",
        ],
    install_requires=[],
    provides=['PScheme'],
    packages = ['PScheme', 'tests'],
    )

