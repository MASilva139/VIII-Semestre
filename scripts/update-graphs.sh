# #!usr/bin/env bash
# set -euo pipefail

# # Gráfica de todas las ramas
# GRAPH=$(git log --graph --decorate --oneline --all)
# # Escapar caracteres para seed
# GRAPH_ESCAPED=$(printf '%s\n' "$GRAPH" | sed -e 's/[&/\]/\\&/g')
# # Sustituir en el README entre los marcadores
# sed -i.bak -e "/<!-- COMMIT_GRAPH_START -->/./<!-- COMMIT_GRAPH_END -->/c\\<!-- GOMMIT_GRAPH_START -->\\
# \`\`\`text\\
# $GRAPH_ESCAPED\\
# \`\`\`\\
# <!-- COMMIT_GRAPH_END -->" README.md

# rm -f README.md.bak
# --------------------------------------------

#!/usr/bin/env bash
set -euo pipefail

# Utilidad: reemplaza el bloque entre marcadores en un archivo con un contenido dado.
# Uso: replace_block <archivo> <inicio> <fin> <contenido>
replace_block() {
    local file="$1"
    local start_marker="$2"
    local end_marker="$3"
    local content="$4"

    if [[ ! -f "$file" ]]; then
        echo "Aviso: $file no existe, omito actualización."
        return 0
    fi

    # Si no existen ambos marcadores, no tocar el archivo
    if ! grep -q "$start_marker" "$file" || ! grep -q "$end_marker" "$file"; then
        echo "Aviso: Marcadores no hallados en $file, omito actualización."
        return 0
    fi

    # Usamos awk para sustituir seguro (compatible GNU/BSD)
    awk -v start="$start_marker" -v end="$end_marker" -v repl="$content" '
        BEGIN { inblock=0 }
        {
            if (index($0, start)) {
                print start
                print repl
                inblock=1
                next
            }
            if (index($0, end)) {
                inblock=0
                print end
                next
            }
            if (!inblock) print
        }
    ' "$file" > "$file.tmp"

    mv "$file.tmp" "$file"
}

# Genera el grafo ASCII; $1 son refs explícitas (opcional)
gen_graph() {
    # --decorate para ver nombres de ramas, --graph para ramas/merges, --oneline compacto
    # Si pasan refs (ramas) por argumento, se usa; si no, --all
    if [[ "$#" -gt 0 ]]; then
        git log --graph --decorate --oneline "$@"
    else
        git log --graph --decorate --oneline --all
    fi
}

# Escapa contenido para poner en bloque markdown triple-backtick
as_code_block() {
    local payload="$1"
    # No hace falta escapar si lo envolvemos en ```text ... ```
    printf '```text\n%s\n```\n' "$payload"
}

# 1) README de la raíz: TODAS las ramas
if [[ -f "README.md" ]]; then
    ROOT_GRAPH="$(gen_graph)"
    ROOT_BLOCK="$(as_code_block "$ROOT_GRAPH")"
    replace_block "README.md" "<!-- COMMIT_GRAPH_START -->" "<!-- COMMIT_GRAPH_END -->" "$ROOT_BLOCK"
fi

# 2) README de F811-FC: SOLO ramas F811 y T01 (si existen)
if [[ -f "F811-FC/README.md" ]]; then
    # Comprobar si existen las ramas; si no, el comando fallaría. Construimos lista segura.
    REFS=()
    git show-ref --verify --quiet refs/heads/F811 && REFS+=("F811") || true
    git show-ref --verify --quiet refs/remotes/origin/F811 && REFS+=("origin/F811") || true
    git show-ref --verify --quiet refs/heads/T01 && REFS+=("T01") || true
    git show-ref --verify --quiet refs/remotes/origin/T01 && REFS+=("origin/T01") || true

    if [[ "${#REFS[@]}" -eq 0 ]]; then
        F811_GRAPH="(Aún no hay ramas F811/T01 locales o remotas en este repo)"
    else
        F811_GRAPH="$(gen_graph "${REFS[@]}")"
    fi

    F811_BLOCK="$(as_code_block "$F811_GRAPH")"
    replace_block "F811-FC/README.md" "<!-- COMMIT_GRAPH_F811_START -->" "<!-- COMMIT_GRAPH_F811_END -->" "$F811_BLOCK"
fi

# Mostrar diff para debug local (no imprescindible)
echo "Archivos modificados:" || true
git status --porcelain || true
