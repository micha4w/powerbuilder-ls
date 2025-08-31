import subprocess, os, shutil
from typing import cast
import bs4
import md_converter

if not os.path.exists('extracted'):
    os.mkdir('extracted')

    pbman = R'C:\Program Files (x86)\Appeon\PowerBuilder 25.0\Help\pbman.chm'
    code = subprocess.call([R'C:\Program Files\7-Zip\7z.exe', 'x', pbman, '-oextracted'])
    if code != 0:
        shutil.rmtree('extracted')


items = {os.path.splitext(path)[0].lower():[os.path.join('extracted', path), None] for path in os.listdir('extracted')}

def item_exists(key: str):
    return key.lower() in items

def get_help(key):
    path, cached = items[key.lower()]
    if cached is not None: return cached

    with open(path, 'rb') as f:
        html = bs4.BeautifulSoup(f, features='html.parser')

    try:
        md = md_converter.transpile_html(html)
    except Exception as e:
        raise Exception(f"While transpiling {path}:", e)

    # with open('test.md', 'w') as f:
        # f.write(md)

    items[key.lower()][1] = md
    return md