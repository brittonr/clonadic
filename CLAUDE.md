# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

ClonadSheet is an experimental spreadsheet where an LLM  evaluates every formula instead of a traditional formula engine. Built for a Jane Street Hackathon.

## Build & Run Commands

```bash
# Enter development environment (starts Ollama, downloads model)
nix develop

# Build
cabal build

# Run (starts server on http://localhost:8080)
cabal run clonad-demo
# or: nix run

# Format code
nix fmt

# Lint
hlint src/ app/

# Auto-recompile on save
ghcid --command="cabal repl clonad-demo"
```

## Architecture

**Backend (Haskell):**
- `src/Spreadsheet.hs` - Core library: Grid/Cell types, formula evaluation via LLM, dependency tracking, cell reference parsing
- `app/Main.hs` - Scotty web server with REST API, STM-based state management

**Frontend:**
- `static/index.html` - HTMX-powered UI with dark theme

**Key Flow:**
1. User edits cell via frontend
2. `POST /api/cell` receives update
3. If formula (starts with `=`): evaluate via Clonad LLM library
4. `findDependentCells` identifies cells referencing this one
5. Cascading recalculation triggers for all dependents

**API Endpoints:**
- `GET /` - Serve frontend
- `GET /api/grid` - Current grid state + stats
- `POST /api/cell` - Update cell (triggers recalculation)
- `POST /api/recalculate` - Recalculate all formulas
- `GET /api/autocomplete` - Formula completions

## Dependencies

- **Clonad**: Custom Haskell library for LLM integration (located at `../clonad`)
- **Ollama**: Local LLM runtime (auto-started by `nix develop`)
- **Model**: `qwen2.5:0.5b` (lightweight 400MB model)

## Environment Variables

Set automatically by `nix develop`:
- `OLLAMA_HOST=http://localhost:11434`
- `CLONAD_MODEL=qwen2.5:0.5b`
