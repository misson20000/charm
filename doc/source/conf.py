# Configuration file for the Sphinx documentation builder.

# -- Project information

project = 'Charm'
copyright = '2024, misson20000'
author = 'misson20000'

release = '0.1.0'
version = '0.1.0'

# -- General configuration

extensions = [
    'sphinx.ext.duration',
]

templates_path = ['_templates']

# -- Options for HTML output

html_theme = 'sphinx_rtd_theme'

# -- Options for EPUB output
epub_show_urls = 'footnote'
