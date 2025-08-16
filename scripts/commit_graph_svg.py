#!/usr/bin/env python3
import subprocess, html, os, re, sys

# ---------- utilidades ----------
def run(cmd):
    return subprocess.check_output(
        cmd, shell=True, text=True, encoding="utf-8", errors="ignore"
    ).rstrip("\n")

def get_branches():
    # ramas locales y remotas (nombres cortos)
    out = run("git for-each-ref --format='%(refname:short)' refs/heads refs/remotes || echo ''")
    return [b.strip().strip("'").strip() for b in out.splitlines() if b.strip()]

def filtered_refs(regex):
    if not regex:
        return []  # semántica: sin filtro explícito => que el script use --all
    pat = re.compile(regex)
    return [b for b in get_branches() if pat.search(b)]

def git_graph(refs=None, limit=None):
    base = "git log --graph --decorate --oneline"
    if refs and len(refs) > 0:
        cmd = f"{base} " + " ".join(refs)
    else:
        cmd = f"{base} --all"
    if limit:
        cmd += f" -n {int(limit)}"
    try:
        return run(cmd)
    except subprocess.CalledProcessError:
        return "(sin historial)"

def count_commits(refs):
    lines = []
    seen = set()
    for r in refs:
        if r in seen: 
            continue
        seen.add(r)
        try:
            c = run(f"git rev-list --count {r} 2>/dev/null || echo 0")
        except subprocess.CalledProcessError:
            c = "0"
        lines.append(f"{r}: {c} commits")
    if not lines:
        lines = ["(no hay ramas coincidentes)"]
    return lines

def make_svg(lines, out_path, theme="dark"):
    # parámetros visuales
    if theme == "dark":
        bg_color = "#0d1117"
        fg_color = "#c9d1d9"
        title_color = "#58a6ff"
    else:
        bg_color = "#ffffff"
        fg_color = "#24292f"
        title_color = "#0969da"

    font_family = "ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, 'Liberation Mono', 'Courier New', monospace"
    font_size = 14
    line_height = 18
    char_width = 8
    padding = 16

    max_cols = max((len(l) for l in lines), default=1)
    width  = padding*2 + max_cols*char_width
    height = padding*2 + len(lines)*line_height

    def svg_line(y, text, color=fg_color, weight="normal"):
        safe = html.escape(text).replace(" ", "&#160;")
        return f'<text x="{padding}" y="{y}" fill="{color}" font-family="{font_family}" font-size="{font_size}" font-weight="{weight}">{safe}</text>'

    y = padding + font_size
    svg_texts = []
    for line in lines:
        color = title_color if line.startswith("Árbol") or line.startswith("Conteo") else fg_color
        weight = "600" if color == title_color else "normal"
        svg_texts.append(svg_line(y, line, color=color, weight=weight))
        y += line_height

    svg = f"""<?xml version="1.0" encoding="UTF-8"?>
<svg width="{width}" height="{height}" viewBox="0 0 {width} {height}" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="Commit graph">
  <title>Commit graph</title>
  <rect x="0" y="0" width="{width}" height="{height}" fill="{bg_color}" rx="8" ry="8"/>
  {'\n  '.join(svg_texts)}
</svg>
"""
    os.makedirs(os.path.dirname(out_path), exist_ok=True)
    with open(out_path, "w", encoding="utf-8") as f:
        f.write(svg)
    print(f"SVG generado: {out_path} ({len(lines)} líneas)")

# ---------- parámetros por entorno ----------
# BRANCH_REGEX: regex para filtrar ramas (vacío => todas)
# LINE_LIMIT: limitar número de commits en el árbol (opcional)
# OUTPUT: ruta de salida
branch_regex = os.environ.get("BRANCH_REGEX", "")
line_limit   = os.environ.get("LINE_LIMIT", "")
output_path  = os.environ.get("OUTPUT", "assets/commit-graph.svg")

# refs filtradas (si hay regex)
refs = filtered_refs(branch_regex)

# árbol
graph = git_graph(refs=refs, limit=line_limit)
graph_lines = graph.splitlines() if graph else ["(sin historial)"]

# conteo
count_lines = count_commits(refs if branch_regex else get_branches())

# ensamblar contenido
lines = []
lines.append("Árbol de commits")
lines.append("")
lines += graph_lines
lines.append("")
lines.append("Conteo de commits por rama")
lines.append("")
lines += count_lines

# generar SVG
make_svg(lines, output_path, theme=os.environ.get("THEME","dark"))
