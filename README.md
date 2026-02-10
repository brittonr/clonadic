# Clonadic

An spreadsheet where every formula is evaluated by an LLM instead of a traditional formula engine.
The power of the spreadsheet, without the chains of enumerating each formula in code.
Built for Amerihac 2026 as the tech demo of the Clonad™

```
=SUM(A1:A3)  ->  LLM interprets "add these cells"  ->  6 (Maybe)
=IF(A1>5, "big", "small")  ->  LLM understands conditionals  ->  "small"
=A1*B1  ->  LLM performs arithmetic  ->  50
```

## Quick Start

```bash
# Enter development environment (auto-starts Ollama, downloads model)
nix develop

# Run the application
cabal run clonadic

# Open in browser
open http://localhost:8080
```
## Architecture

```
                    +-----------------+
                    |   Browser UI    |
                    |  (index.html)   |
                    +--------+--------+
                             |
                    HTTP/REST API
                             |
                    +--------v--------+
                    |  Scotty Server  |
                    |   (Main.hs)     |
                    +--------+--------+
                             |
              +--------------+--------------+
              |                             |
    +---------v---------+         +---------v---------+
    |   Spreadsheet.hs  |         |     Config.hs     |
    | Grid, Cell types  |         |   TOML parsing    |
    | Formula eval      |         +-------------------+
    | Dependency track  |
    +---------+---------+
              |
    +---------v---------+
    |   Clonad Library  |
    |  (LLM FFI)    |
    +---------+---------+
```

### Backend (Haskell)

| File | Purpose |
|------|---------|
| `src/Spreadsheet.hs` | Core types (Grid, Cell, CellValue), formula evaluation, dependency tracking, autocomplete |
| `src/Config.hs` | TOML configuration parsing |
| `app/Main.hs` | Scotty web server, REST API, STM state management |

### Key Types

```haskell
type Coord = (Int, Int)  -- (row, col), 1-indexed

data CellValue
  = CellEmpty
  | CellNumber Double
  | CellText Text
  | CellBoolean Bool
  | CellError Text

data Cell = Cell
  { cellValue   :: CellValue
  , cellFormula :: Maybe Text
  , cellDisplay :: Text
  }

type Grid = Map Coord Cell
```

## API Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/api/grid` | Get current grid state and stats |
| POST | `/api/cell` | Update a cell (`{row, col, value}`) |
| POST | `/api/recalculate` | Recalculate all formula cells |
| GET | `/api/stats` | Get operation statistics |
| GET | `/api/autocomplete?input=...` | Get formula suggestions |

## Configuration

Edit `config.toml`:

```toml
[server]
host = "localhost"
port = 8080

[llm]
ollama_host = "http://localhost:11434"
model = "qwen2.5:0.5b"
temperature = 0.0 

[stats]
tokens_per_operation = 300
cost_per_operation = 0.003

[grid]
default_rows = 20
default_cols = 10
```

## Development Commands

```bash
# Build
cabal build

# Run
cabal run clonadic

# Format code
nix fmt

# Lint
hlint src/ app/

# Auto-recompile on file changes
ghcid --command="cabal repl clonadic"
```

## How Formula Evaluation Works

1. User enters `=A1+B1` in cell C1
2. `extractCellRefs` parses formula, finds references: `[(1,1), (1,2)]`
3. Context built: `"A1=5, B1=10"`
4. LLM prompt constructed:
   ```
   Evaluate: =A1+B1 with A1=5, B1=10
   ```
5. LLM responds: `15`
6. Response parsed into `CellNumber 15`
7. Cell updated, display shows "15"
8. `findDependentCells` finds any cells referencing C1
9. Dependents recalculated (cascading)

These are hints for autocomplete. The LLM interprets them, so creative formulas may also work.

## Technology Stack

| Component | Technology |
|-----------|------------|
| Backend | Haskell (GHC 2024) |
| Web Framework | Scotty |
| State Management | STM (Software Transactional Memory) |
| Configuration | TOML (tomland) |
| JSON | Aeson |
| LLM Integration | Clonad (custom library) |
| LLM Runtime | Ollama |
| Model | qwen2.5:0.5b |
| Frontend | Vanilla JavaScript |
| Build System | Cabal + Nix |

## Dependencies

- **Clonad**: Custom Haskell library for LLM FFI
