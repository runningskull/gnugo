# gtp-commands.sed (edit in -*-shell-script-*- mode)
# Author: Thien-Thi Nguyen <ttn@gnu.org>

# Look for function headers.
/\* Function: /,/^{/!d

# Remove cruft.
/^static int/d
/^{/d
/^ \*\//d
s/(char.*)//g

# Hold comment lines, deleting them from pattern space for now.
/.\*/{
 s/^..//
 H
 d
}

# When we see the function name, merge hold space, in the process
# generating proper texinfo @cindex, @item and @example formatting.
# We use repeated `x' commands instead of the simpler `i' to avoid
# requiring a `d' (which would render this script non-composable).
/^gtp_/{
 s/^\(.*\)$/@cindex \1\
@item \1\
\
@example/
 G
 x
 s/.*/\
@end example\
/
 x
 G
 x
 s/.*//
 x
}

# gtp-commands.sed ends here


