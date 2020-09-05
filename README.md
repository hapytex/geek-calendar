# geek-calendar

[![Build Status of the package by Travis](https://travis-ci.com/hapytex/geek-calendar.svg?branch=master)](https://travis-ci.com/hapytex/geek-calendar)

A list of geeky events we can celebrate. For example birthdays of important
scientists, releases of science fiction movies, events in fantasy stories, etc.
The *GitHub pages* contain a [`.ics` calendar file](https://hapytex.github.io/geek-calendar/calendar.ics).

The data is structured into several directories.

# File structure

For each important researcher, a directory is made in the [**`birthdays/`**](birthdays/)
directory. The directory name has the form `yyyymmdd-slug_of_person-name`. With
`yyyymmdd` the birthday according to the *Gregorian* calendar of that person.
The directory contains two files `name` that contains the full name of the
person, and `bio.md` that contains a *Markdown* formatted file that contains a
short biography of that person.

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

