#!/usr/bin/env python3
"""Generate vortel-lsp catalog JSON from Helix languages.toml."""

from __future__ import annotations

import argparse
import json
import tomllib
from pathlib import Path
from typing import Any


def normalize_server_entry(entry: Any) -> dict[str, Any]:
    if isinstance(entry, str):
        return {"name": entry, "only": [], "except": []}
    if isinstance(entry, dict):
        return {
            "name": entry.get("name"),
            "only": list(entry.get("only-features", [])),
            "except": list(entry.get("except-features", [])),
        }
    return {"name": None, "only": [], "except": []}


def normalize_file_type(entry: Any) -> dict[str, str] | None:
    if isinstance(entry, str):
        return {"ext": entry}
    if isinstance(entry, dict) and "glob" in entry:
        return {"glob": entry["glob"]}
    return None


def normalize_server_config(name: str, config: dict[str, Any]) -> dict[str, Any]:
    return {
        "name": name,
        "command": config.get("command"),
        "args": list(config.get("args", [])),
        "environment": dict(config.get("environment", {})),
        "config": config.get("config"),
        "timeout": int(config.get("timeout", 20)),
        "required_root_patterns": list(config.get("required-root-patterns", []) or []),
    }


def normalize_language_config(language: dict[str, Any]) -> dict[str, Any]:
    file_types = [
        item
        for item in (
            normalize_file_type(entry) for entry in language.get("file-types", [])
        )
        if item is not None
    ]
    servers = [
        item
        for item in (
            normalize_server_entry(entry)
            for entry in language.get("language-servers", [])
        )
        if item.get("name")
    ]
    return {
        "name": language.get("name"),
        "language_id": language.get("language-id"),
        "scope": language.get("scope"),
        "file_types": file_types,
        "roots": list(language.get("roots", [])),
        "servers": servers,
    }


def generate_catalog(source_path: Path) -> dict[str, Any]:
    with source_path.open("rb") as fh:
        raw = tomllib.load(fh)

    server_raw = raw.get("language-server", {})
    language_raw = raw.get("language", [])

    servers: dict[str, Any] = {}
    for name, config in sorted(server_raw.items()):
        if not isinstance(config, dict):
            continue
        servers[name] = normalize_server_config(name, config)

    languages = [
        normalize_language_config(language)
        for language in language_raw
        if isinstance(language, dict)
    ]
    languages.sort(key=lambda item: item.get("name") or "")

    return {
        "source": str(source_path),
        "servers_count": len(servers),
        "languages_count": len(languages),
        "servers": servers,
        "languages": languages,
    }


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--source",
        type=Path,
        default=Path(__file__).resolve().parents[2] / "languages.toml",
        help="Path to Helix languages.toml",
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=Path(__file__).resolve().parents[1] / "vortel-lsp-catalog.json",
        help="Output catalog JSON path",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    catalog = generate_catalog(args.source)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    with args.output.open("w", encoding="utf-8") as fh:
        json.dump(catalog, fh, ensure_ascii=False, indent=2)
        fh.write("\n")
    print(f"wrote {args.output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
