# %%
import csv
import os
from pathlib import Path

import anthropic
from dotenv import load_dotenv

load_dotenv("/Users/jhelvy/gh/web/jhelvy_quarto/.env")

# %%
BLOG_DIR = Path("/Users/jhelvy/gh/web/jhelvy_quarto/blog/2026-05-12-quarto-optimal-claude-output")
OUTPUTS_DIR = BLOG_DIR / "experiments" / "outputs"
DATA_DIR = BLOG_DIR / "data"
CSV_PATH = DATA_DIR / "token_results.csv"

OUTPUTS_DIR.mkdir(parents=True, exist_ok=True)
DATA_DIR.mkdir(parents=True, exist_ok=True)

# %%
MODEL = "claude-haiku-4-5"

DOCUMENT_SPEC = """
A summary report on electric vehicle charging standards, comparing CCS
(Combined Charging System), CHAdeMO, and NACS (North American Charging
Standard, formerly Tesla). The report must include:

1. An introduction explaining the landscape
2. A comparison table with columns: Standard, Origin, Max Power (kW),
   Connector Shape, Adoption Region
3. A pros and cons section for each of the three standards
4. A conclusion with a practical recommendation
""".strip()

# %%
CONDITIONS = [
    (
        "direct_html",
        "direct",
        "html",
        (
            "Produce a COMPLETE, self-contained HTML file (DOCTYPE, <head> with "
            "embedded CSS, <body>) for the following document. "
            "Output ONLY the raw HTML — no markdown fences, no explanation.\n\n"
            f"{DOCUMENT_SPEC}"
        ),
        "html",
    ),
    (
        "direct_markdown",
        "direct",
        "markdown",
        (
            "Produce a plain Markdown document for the following. "
            "Output ONLY the Markdown — no code fences, no explanation.\n\n"
            f"{DOCUMENT_SPEC}"
        ),
        "md",
    ),
    (
        "direct_latex",
        "direct",
        "pdf",
        (
            "Produce a COMPLETE, compilable LaTeX document (\\documentclass, "
            "\\usepackage declarations, \\begin{document}...\\end{document}) for "
            "the following. Output ONLY the LaTeX — no markdown fences, no "
            "explanation.\n\n"
            f"{DOCUMENT_SPEC}"
        ),
        "tex",
    ),
    (
        "direct_docx_xml",
        "direct",
        "word",
        (
            "Produce the complete Office Open XML content for the "
            "word/document.xml file inside a .docx package for the following "
            "document. Include the full XML with proper namespace declarations. "
            "Output ONLY the XML — no markdown fences, no explanation.\n\n"
            f"{DOCUMENT_SPEC}"
        ),
        "xml",
    ),
    (
        "quarto_html",
        "quarto",
        "html",
        (
            "Produce a Quarto (.qmd) document with 'format: html' in the YAML "
            "front matter for the following document. Use standard Quarto/Markdown "
            "syntax only — no raw HTML blocks. "
            "Output ONLY the .qmd content — no code fences, no explanation.\n\n"
            f"{DOCUMENT_SPEC}"
        ),
        "qmd",
    ),
    (
        "quarto_pdf",
        "quarto",
        "pdf",
        (
            "Produce a Quarto (.qmd) document with 'format: pdf' in the YAML "
            "front matter for the following document. Use standard Quarto/Markdown "
            "syntax — no raw LaTeX blocks. "
            "Output ONLY the .qmd content — no code fences, no explanation.\n\n"
            f"{DOCUMENT_SPEC}"
        ),
        "qmd",
    ),
    (
        "quarto_word",
        "quarto",
        "word",
        (
            "Produce a Quarto (.qmd) document with 'format: docx' in the YAML "
            "front matter for the following document. Use standard Quarto/Markdown "
            "syntax. "
            "Output ONLY the .qmd content — no code fences, no explanation.\n\n"
            f"{DOCUMENT_SPEC}"
        ),
        "qmd",
    ),
    (
        "quarto_markdown",
        "quarto",
        "markdown",
        (
            "Produce a Quarto (.qmd) document with 'format: gfm' in the YAML "
            "front matter for the following document. Use standard Quarto/Markdown "
            "syntax. "
            "Output ONLY the .qmd content — no code fences, no explanation.\n\n"
            f"{DOCUMENT_SPEC}"
        ),
        "qmd",
    ),
]

# %%
client = anthropic.Anthropic(api_key=os.environ.get("ANTHROPIC_API_KEY"))

# %%
CSV_FIELDS = ["condition", "format_type", "output_type", "input_tokens", "output_tokens", "char_count"]
rows = []

for condition_id, format_type, output_type, prompt, ext in CONDITIONS:
    output_file = OUTPUTS_DIR / f"{condition_id}.{ext}"
    if output_file.exists():
        print(f"  [skip] {condition_id} — output already exists")
        continue

    print(f"  [run ] {condition_id} …", end=" ", flush=True)
    response = client.messages.create(
        model=MODEL,
        max_tokens=8192,
        messages=[{"role": "user", "content": prompt}],
    )

    raw_text = next((b.text for b in response.content if b.type == "text"), "")
    output_file.write_text(raw_text, encoding="utf-8")

    row = {
        "condition": condition_id,
        "format_type": format_type,
        "output_type": output_type,
        "input_tokens": response.usage.input_tokens,
        "output_tokens": response.usage.output_tokens,
        "char_count": len(raw_text),
    }
    rows.append(row)
    print(f"in={row['input_tokens']:,}  out={row['output_tokens']:,}  chars={row['char_count']:,}")

# %%
existing = {}
if CSV_PATH.exists():
    with CSV_PATH.open(newline="") as f:
        for r in csv.DictReader(f):
            existing[r["condition"]] = r

for row in rows:
    existing[row["condition"]] = row

with CSV_PATH.open("w", newline="") as f:
    writer = csv.DictWriter(f, fieldnames=CSV_FIELDS)
    writer.writeheader()
    writer.writerows(existing.values())

print(f"\nResults written to {CSV_PATH}")
