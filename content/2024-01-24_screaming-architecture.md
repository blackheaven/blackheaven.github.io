+++
title = "Screaming Architecture"
date = 2024-01-24
draft = false
path = "2024-01/screaming-architecture"

[taxonomies]
categories = ["Software engineering"]
tags = ["software engineering", "product", "design"]
+++

During my participation to the last [Global Day of Code Retreat](@/2023-11-05_gdcr-summary.md)
one of the participant came into our room (where we were doing some software teaming,
but we'll talk about it in another log), looking at the hard-to-read code (due
to the constraint) and he joked about the [Screaming Architecture](https://blog.cleancoder.com/uncle-bob/2011/09/30/Screaming-Architecture.html).

The idea is to organize the code so, when you open your project you know its propose.

For example:

```
src
└── X
    ├── Entities.hs
    ├── InterfaceAdapters
    │   └── API.hs
    ├── UseCases.hs
    └── Utils
        ├── EventSourcing.hs
        ├── Random.hs
        └── Servant.hs
```

does not fit (yes, it is taken from [last log on Abaks](@/2023-07-02_abaks-testing-strategy.md)).

Sure, it does not put the technology upfront (unlike what's described in the article),
but it talks more about the architecture style I have picked.

On another hand, [hackage-server](https://github.com/haskell/hackage-server) does better at it:

```
src
└── Distribution
    ├── Server
    │   ├── Features
    │   │   ├── AdminFrontend.hs
    │   │   ├── AdminLog.hs
    │   │   ├── AnalyticsPixels
    │   │   │   └── State.hs
    │   │   ├── AnalyticsPixels.hs
    │   │   ├── Browse
    │   │   │   ├── ApplyFilter.hs
    │   │   │   ├── Options.hs
    │   │   │   └── Parsers.hs
    │   │   ├── Browse.hs
    │   │   ├── BuildReports
    │   │   │   ├── Backup.hs
    │   │   │   ├── BuildReport.hs
    │   │   │   ├── BuildReports.hs
    │   │   │   ├── Render.hs
    │   │   │   └── State.hs
    │   │   ├── BuildReports.hs
    │   │   ├── Core
    │   │   │   ├── Backup.hs
    │   │   │   └── State.hs
    │   │   ├── Core.hs
    │   │   ├── Crash.hs
```

Actually, this architecture style favors [vertical slicing](https://en.wikipedia.org/wiki/Vertical_slice)
which enforces the grouping by business concern instead of technical layer.

It's a powerful way to look at code organization, which fits well with Haskell
it is a very concise and let you structure freely your code (unlike some other
programming languages which enforce units per file, e.g. Java and classes).

However, it can be an issue when you have always worked on framework-driven projects.

If I have to take back the article, I remember a talk where the speaker showed
the view-from-above plan of a Christian church to make his point.

The thing is, not all churches are T-shaped (e.g. Helsinki has a [round church](https://en.wikipedia.org/wiki/Temppeliaukio_Church)),
and not everyone knows what a Christian church look like from above (or can
figure it out conceptually).
You have to align shared preconceptions/assumptions (which are part of the culture)
to have an effective Screaming Architecture.
