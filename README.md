# geek-calendar

[![Build Status of the package by GitHub actions](https://github.com/hapytex/geek-calendar/actions/workflows/build-ci.yml/badge.svg)](https://github.com/hapytex/geek-calendar/actions/workflows/build-ci.yml)

A list of geeky events we can celebrate. For example birthdays of important
scientists, releases of science fiction movies, events in fantasy stories, etc.
The *GitHub pages* contain a [`.ics` calendar file :calendar:](https://hapytex.github.io/geek-calendar/geek-calendar.ics).

The data is structured into several directories.

# File structure

For each important *person* (researcher, actor, comic book artist, etc.),
a directory is made in the [**`birthday/`**](birthday/)
directory. The directory name has the form `yyyymmdd-slug_of_person_name`. With
`yyyymmdd` the birthday according to the *Gregorian* calendar of that person.
The directory contains two files `name` that contains the full name of the
person, and `bio.md` that contains a *Markdown* formatted file that contains a
short biography of that person.

For each *universe*, a directory is made in the [**`universe/`**](universe/)
directory. The directory name is the slug of the universe. In the directory, there
are three files: `name` will contain the name of that universe, `emoji`, an optional
file contains an emoji linked to that universe, and the directory can also contain
`links`.

For events with a *fixed day*, the [**`fixedday/`**](fixedday/) directory is used.
In this directory subdirectories per universe are created, and for each of these
subdirectories, a directory is used per fixed day event. This directory has the form
`yyyymmdd-slug_of_event_name`. In this directory a file named `name` will contain the
real name of the event, a file `description.md` will contain a short description of the
event in *Markdown* and optionally a file named `why.md` that contains the reason
why that specific date was picked.

## Files in each directory

Each directory can contain a file named `links` which contains a list of links
separated by a new line.

Furthermore a directory can contain a file named `notes.md` that contains a set
of notes written with markdown syntax.

Some events happen when the *Julian calendar* was used. By adding a file
`julian` in the directory, a note will be added that specifies the date
according to the Julian calendar.

## `geek-calendar` is not *safe* Haskell

The package itself is safe, but it uses modules like `Text.Blaze.Html` that are
not safe.

## Contribute

You can contribute by making a pull request on the [*GitHub
repository*](https://github.com/hapytex/geek-calendar).

You can contact the package maintainer by sending a mail to
[`hapytexeu+gh@gmail.com`](mailto:hapytexeu+gh@gmail.com).

