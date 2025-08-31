from typing import cast
import bs4
import re


# Ugly code, but it does the job. just the job, not a bit more...

# TODO: do something smarter fo the urls?

def transpile_text(tag: bs4.Tag | bs4.NavigableString) -> str:
    if isinstance(tag, bs4.NavigableString):
        text = tag.get_text(' ', strip=True)
        text = text.replace('\\', '\\\\')

        # def repl(match: re.Match[str]) -> str:
        #     return match.group(1)+'\\<' if match.group(1) else '&lt;'
        # text = re.sub(r'(\s?)<(?=\w.*>)', repl, text)
        text = re.sub(r'<(?=\w.*>)', '\\<', text)
        text = re.sub(r'(?m)^\s*#', '\\#', text)
        text = text.replace('*', '\\*')
        text = text.replace('_', '\\_')
        return text

    parts = []

    for child in tag.children:
        match child:
            case bs4.Tag():
                if child.name == 'a':
                    if 'name' in child.attrs:
                        parts.append(f'<a name={child.attrs['name']}></a>')
                    else:
                        parts.append(f'[{transpile_text(child)}](https://docs.appeon.com/pb2025/powerscript_reference/{child.attrs['href']})')

                elif child.name == 'span' and 'olinkdocname' in child.attrs['class']:
                    parts.append(transpile_text(child))
                elif child.name == 'span' and 'bold' in child.attrs['class']:
                    strong = child.find('strong')
                    assert strong.text == child.text
                    parts.append('**' + transpile_text(strong) + '**')
                elif child.name == 'b' or child.name == 'strong':
                    parts.append('*' + transpile_text(child) + '*')
                elif child.name == 'span' and 'emphasis' in child.attrs['class']:
                    em = child.find('em')
                    assert em.text == child.text
                    parts.append('*' + transpile_text(em) + '*')
                elif child.name == 'i' or child.name == 'em':
                    parts.append('*' + transpile_text(child) + '*')
                elif child.name == 'code':
                    parts.append('`' + child.text + '`')
                elif child.name == 'span' and 'inlinemediaobject' in child.attrs['class']:
                    src = child.find('img').attrs['src']
                    parts.append(f'![{src}](https://docs.appeon.com/pb2025/powerscript_reference/{src})')
                else:
                    assert False, child
            case bs4.NavigableString():
                parts.append(transpile_text(child))

    text = ' '.join(parts)

    text = re.sub(r' +', ' ', text)
    text = re.sub(r'(?m)^ | $', '', text)
    return text.strip()


def transpile_p(p: bs4.Tag):
    assert p.name == 'p', p

    return transpile_text(p) + '\n'


def transpile_list(list: bs4.Tag) -> str:
    ordered = 'orderedlist' in list.attrs['class']
    if ordered:
        assert list.find('ol').attrs['type'] == '1'

    md = ''
    items = []
    simple = True

    lis = list.find_all('li', {'class': 'listitem'})
    for i, li in enumerate(lis):
        items.append([])

        for child in li.children:
            if child.name in ['pre', 'br'] or \
                    (child.name == 'div' and
                     any(c in child.attrs['class'] for c in ['figure', 'note'])):
                simple = False

            items[-1].append(transpile_item(child))

    if simple:
        max_len = len(str(len(lis)))
        def item_start(i): return f'{i+1:{max_len}}. ' if ordered else '- '
        item_break = '\n' + ' '*(2 if ordered else 2+max_len)

        for i, item in enumerate(items):
            for j, line in enumerate(item):
                md += item_start(i) if j == 0 else '\n'+item_break
                md += line.strip().replace('\n', item_break)

            md = md.rstrip() + '\n'
    else:
        md += f'<{'uo'[ordered]}l>\n'
        for item in items:
            md += ' <li>\n\n'
            for line in item:
                md += re.sub(r'(?m)^(?!$)', '  ', line)

            md += ' </li>\n'

        md += f'</{'uo'[ordered]}l>\n'

    return md


def transpile_table(table: bs4.Tag) -> str:
    assert table.name == 'table'

    md = ""

    simple = True
    rows: list[list[str]] = []

    for tr in table.find_all('tr'):
        rows.append([])

        tr = cast(bs4.Tag, tr)
        for i, tc in enumerate(tr.contents):
            tc = cast(bs4.Tag, tc)

            tags = list(tc.find_all(True, recursive=False))
            if all(child.name in [None, 'span', 'a', 'i'] for child in tc.contents):
                rows[-1].append(transpile_text(tc).strip())
            elif len(tags) == 1 and tags[0].name == 'p':
                rows[-1].append(transpile_text(tags[0]).strip())
            else:
                simple = False
                field = ''
                for item in tc.contents:
                    field += transpile_item(cast(bs4.Tag, item))
                rows[-1].append(field)

    if simple:
        # "| Month    | Savings |"
        # "| -------- | ------- |"
        # "| January  | $250    |"
        # "| February | $80     |"
        # "| March    | $420    |"

        rows = [[re.sub(r' +', ' ', line.replace('\n', ' ')) for line in row] for row in rows]

        lengths = [max(map(len, col)) for col in zip(*rows)]
        for i, row in enumerate(rows):
            md += '|'+'|'.join(f' {f:{l}} ' for f, l in zip(row, lengths))+'|\n'
            if i == 0:
                md += '|'+'|'.join(f' {'-'*l} ' for l in lengths)+'|\n'

    else:
        # <table>
        # <tr><th>
        #  Month
        # </th><th>
        #  Savings
        # </th></tr>

        # <tr><td>
        #  January
        # </td><td>
        #  $250
        # </td></tr>
        # </table>
        md += '<table>'

        for i, row in enumerate(rows):
            md += '\n<tr>'

            for field in row:
                md += f'<t{'hd'[i > 0]}>\n\n'
                md += re.sub(r'(?m)^(?!$)', ' ', field)
                md = md.strip('\n') + '\n'
                md += f'</t{'hd'[i > 0]}>'

            md += '<tr>\n'

        md += '<table>\n'

    return md


def transpile_item(item: bs4.Tag) -> str:
    md = ''

    if item.name == 'p':
        children = list(item.children)
        if len(children) == 1 and children[0].name == 'span' and 'bold' in children[0].attrs['class']:
            heading = item.find('strong')
            heading = cast(bs4.Tag, heading)
            md += f'#### {transpile_text(heading)}\n'

            assert heading.text == item.text
        else:
            md += transpile_p(item) + '\n'

    elif item.name == 'table':
        assert 'simplelist' in item.attrs['class'], item.attrs['class']

        for row in item.find_all('td'):
            assert not row.find(True), row
            md += '- ' + transpile_text(row).strip() + '\n'

        md += '\n'

    elif item.name == 'div':
        if 'simplesect' in item.attrs['class']:
            md += transpile_section(item)

        elif 'table' in item.attrs['class'] or 'informaltable' in item.attrs['class']:
            table = item.find('table')

            md += transpile_table(table)
            md += '\n'

        elif 'toc' in item.attrs['class']:
            for toc in item.find_all('span', {'class': 'section'}):
                md += f' - {transpile_text(toc)}\n'

            md += '\n'

        elif 'itemizedlist' in item.attrs['class'] or 'orderedlist' in item.attrs['class']:
            md += transpile_list(item) + '\n'

        elif any(s in item.attrs['class'] for s in ['note', 'tip', 'important', 'warning', 'caution']):
            kind = next(s for s in ['note', 'tip', 'important', 'warning', 'caution'] if s in item.attrs['class'])
            md += f'> [!{kind.upper()}]\n'
            for child in item.children:
                child = cast(bs4.Tag, child)
                if child.name == 'h3':
                    md += f'> ### {transpile_text(child)}\n'
                elif child.name in [None, 'p', 'pre'] or (child.name == 'div' and 'itemizedlist' in child.attrs['class']):
                    p = transpile_item(child).strip()
                    md += re.sub(r'(?m)^', '> ', p) + '\n'
                else:
                    raise Exception(f"Unexpected child tag for note div {child}")

        elif 'figure' in item.attrs['class']:
            title = transpile_text(item.find('p', {'class': 'title'}))
            img = item.find('img').attrs['src']

            md += f'![{title.replace('\n', ' ').replace('  ', ' ')}](https://docs.appeon.com/pb2025/powerscript_reference/{img})\n'
        else:
            raise Exception(f"Unexpected class name for a div child of section div {item.attrs['class']}")

    elif item.name == 'pre':
        assert 'programlisting' in item.attrs['class']
        md += f'```powerbuilder\n{item.text.strip('\n')}\n```\n\n'
    elif item.name == 'br':
        pass
    elif item.name is None:
        if item.text.strip() != '':
            md += transpile_text(item)
            if not md.endswith('\n'):
                md += '\n'
    else:
        raise Exception(f"Unexpected child tag for section div {item}")

    return md


def transpile_section(section: bs4.Tag, toplevel=False) -> str:
    md = ""

    header = section.find('div', {'class': 'titlepage'}).extract()
    title = transpile_text(header.find(attrs={'class': 'title'}))
    subtitle = header.find(attrs={'class': 'subtitle'})

    md += '## ' if toplevel else '### '
    md += title
    if subtitle:
        md += f' ({transpile_text(subtitle)})'
    md += '\n\n'

    for child in section.children:
        md += transpile_item(cast(bs4.Tag, child))

    return md


def transpile_html(html: bs4.BeautifulSoup) -> str:
    body = html.find('body')
    if not body:
        raise Exception('no body')

    section = body.find('div', {'class': 'section'})
    if not section:
        raise Exception('no section')

    section = cast(bs4.Tag, section)
    md = transpile_section(section, toplevel=True)
    md = md.replace('”', '"')
    md = md.replace('“', '"')

    return md
