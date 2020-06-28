# geek-calendar

A list of geeky events we can celebrate. For example birthdays of important
scientists, releases of science fiction movies, events in fantasy stories, etc.

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
