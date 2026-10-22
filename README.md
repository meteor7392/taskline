# taskline

`taskline` is a small command-line task manager that speaks the
todo.txt format. Tasks live in a plain text file, so they stay readable,
diffable, and easy to sync.

## Features

- Parse and render todo.txt lines (priority, creation/completion dates,
  contexts `@`, projects `+`, `due:` dates)
- Add, complete, and delete tasks by number
- Filter lists by project, context, priority, or completion state
- Filter and sort by due date, including overdue tasks
- Edit task descriptions and priorities
- Full-text search across descriptions
- Summary statistics

## Build

Requires GHC and Cabal.

```sh
cabal build
cabal test
cabal run taskline -- help
```

## Usage

```sh
# Add tasks
taskline add "Buy milk @home +chores due:2024-01-02"
taskline add "(A) Call mom @phone"

# List pending tasks
taskline list

# Filter
taskline list --context home
taskline list --priority A --all
taskline list --due 2024-01-02
taskline list --due-before 2024-02-01
taskline list --overdue --sort due

# Complete, edit, or delete by number (from `list`)
taskline done 1
taskline pri 2 B
taskline depri 2
taskline due 1 2024-02-02
taskline edit 1 "Buy oat milk @home +chores due:2024-02-02"
taskline delete 2

# Search and stats
taskline search milk
taskline stats
```

Storage defaults to `~/.taskline/todo.txt`. Override with `--file PATH`
or the `TASKLINE_FILE` environment variable.

## Format

Each line follows todo.txt conventions:

```
x 2024-01-01 (B) 2023-12-31 Finish report @work +office due:2024-01-15
```

- Leading `x ` marks a task complete, followed by its completion date.
- `(A)` is the priority.
- A bare date is the creation date.
- `@context` and `+project` tags are extracted from the description.
- `due:YYYY-MM-DD` records a due date.
