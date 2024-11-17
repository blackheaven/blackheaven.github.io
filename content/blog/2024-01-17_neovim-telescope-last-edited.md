+++
title = "NeoViM: Improve Telescope (last edited) find files"
date = 2024-01-17
draft = false
path = "2024-01/neovim-telescope-last-edited"

[taxonomies]
categories = ["dev"]
tags = ["dev", "development", "flow", "tools", "neovim", "lua"]
+++

Sometimes ago I have stated I was using [ViM](@/blog/2023-08-06_vim.md).

After [Bram Moolenaar's death](https://groups.google.com/g/vim_announce/c/tWahca9zkt4),
[ViM's future was changed](https://groups.google.com/g/vim_dev/c/dq9Wu5jqVTw)
and I could not hope for [Tree-sitter integration](https://tree-sitter.github.io/tree-sitter/)
anymore, so, after a while, I have decided to move to [NeoViM](https://neovim.io/).

I usually reflect on my workflow early in the morning, before the breakfast,
one of the thing I have noticed, using [Telescope](https://github.com/nvim-telescope/telescope.nvim)
to find/open files, is my tendency to re-open the same files over and over during
a coding session.

For example, I open `A.hs` to mainly work on it, then temporarily open `B.hs`
to check or change something, close it, keep working on `A.hs`, then open `C.hs`
change something, close it, keep working on `A.hs`, re-open `B.hs`, etc.

I have tried [telescope-recent-files](https://github.com/smartpde/telescope-recent-files) / [telescope-all-recent](https://github.com/prochri/telescope-all-recent.nvim),
but they were too limited (displaying only recent files), or heavy (requiring SQLite),
so I have decided to come up with my own solution.

To build a Telescope you need few things:

* A picker which is the input list (e.g. files, lines, history, git commits, etc.)
* A sorter to arrange results for the picker
* Eventually a previewer to print something in the panel

Let's start with the configuration:

```lua
require('telescope').setup {
    defaults = {
        file_sorter = opened_files_sorter,
    },
}
```

A `Sorter` is actually an object:

```lua
local function opened_files_sorter(opts)
    opts = opts or {}

    local sorters = require("telescope.sorters")

    local fuzzy_sorter = sorters.get_fzy_sorter(opts)

    return sorters.new {
        scoring_function = function(_, prompt, line, entry, cb_add, cb_filter)
            -- here will be the logic
        end,
        highlighter = fuzzy_sorter.highlighter,
    }
end
```

A `Sorter` is composed by:

* A scoring function which mainly takes the prompt (what we are looking for), the line and the entry we are trying to sort (provided by the picker)
* A highlighting function

I have decided to take de builtin fuzzer sorter as basis.

Let's rely on it as first implementation:

```lua
local function opened_files_sorter(opts)
    opts = opts or {}

    local sorters = require("telescope.sorters")

    local fuzzy_sorter = sorters.get_fzy_sorter(opts)

    return sorters.new {
        scoring_function = function(_, prompt, line, entry, cb_add, cb_filter)
            local FILTERED = -1
            local filepath = vim.fn.resolve(vim.fn.getcwd() .. '/' .. entry.value)

            local base_score = fuzzy_sorter:scoring_function(prompt, line, entry, cb_add, cb_filter)
            if base_score == FILTERED then
                return FILTERED
            end

            return base_score
            end
        end,
        highlighter = fuzzy_sorter.highlighter,
    }
end
```

Then we should collect opened files:

```lua
local opened_files_since_start = {}
local function update_opened_files()
    local current_file = vim.fn.expand('%:p')
    opened_files_since_start[current_file] = true
end

-- In config
local autocmd = vim.api.nvim_create_autocmd
autocmd('BufReadPost', {
    pattern = '',
    callback = update_opened_files
})
```
so we can change the score regarding opened files:

```lua
local function opened_files_sorter(opts)
    opts = opts or {}

    local sorters = require("telescope.sorters")

    local fuzzy_sorter = sorters.get_fzy_sorter(opts)

    return sorters.new {
        scoring_function = function(_, prompt, line, entry, cb_add, cb_filter)
            local FILTERED = -1
            local filepath = vim.fn.resolve(vim.fn.getcwd() .. '/' .. entry.value)

            local base_score = fuzzy_sorter:scoring_function(prompt, line, entry, cb_add, cb_filter)
            if base_score == FILTERED then
                return FILTERED
            end

            if opened_files_since_start[filepath] then
                return 0.001 * base_score
            else
                return base_score
            end
        end,
        highlighter = fuzzy_sorter.highlighter,
    }
end
```

So far so good, to limit the results, I want to filter-out the currently opened files:

```lua
local function opened_files_sorter(opts)
    opts = opts or {}

    local sorters = require("telescope.sorters")

    local fuzzy_sorter = sorters.get_fzy_sorter(opts)

    return sorters.new {
        scoring_function = function(_, prompt, line, entry, cb_add, cb_filter)
            local FILTERED = -1
            local filepath = vim.fn.resolve(vim.fn.getcwd() .. '/' .. entry.value)

            for _, buf in ipairs(vim.fn.getbufinfo({ buflisted = 1 })) do
                if buf.windows and #buf.windows > 0 and buf.name == filepath then
                    return FILTERED
                end
            end

            local base_score = fuzzy_sorter:scoring_function(prompt, line, entry, cb_add, cb_filter)
            if base_score == FILTERED then
                return FILTERED
            end

            if opened_files_since_start[filepath] then
                return 0.001 * base_score
            else
                return base_score
            end
        end,
        highlighter = fuzzy_sorter.highlighter,
    }
end
```

And finally, I want to get rid of binary files (i.e. file extensions already listed in `wildignore`):

```lua
local function opened_files_sorter(opts)
    opts = opts or {}

    local sorters = require("telescope.sorters")

    local fuzzy_sorter = sorters.get_fzy_sorter(opts)

    return sorters.new {
        scoring_function = function(_, prompt, line, entry, cb_add, cb_filter)
            local FILTERED = -1
            local filepath = vim.fn.resolve(vim.fn.getcwd() .. '/' .. entry.value)

            for _, buf in ipairs(vim.fn.getbufinfo({ buflisted = 1 })) do
                if buf.windows and #buf.windows > 0 and buf.name == filepath then
                    return FILTERED
                end
            end

            for _, pat in ipairs(vim.split(vim.o.wildignore, ',')) do
                if match_glob(pat, filepath) then
                    return FILTERED
                end
            end

            local base_score = fuzzy_sorter:scoring_function(prompt, line, entry, cb_add, cb_filter)
            if base_score == FILTERED then
                return FILTERED
            end

            if opened_files_since_start[filepath] then
                return 0.001 * base_score
            else
                return base_score
            end
        end,
        highlighter = fuzzy_sorter.highlighter,
    }
end
```
