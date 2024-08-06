Structure & Display
===================

Structure in Charm is represented as a tree of **nodes**. Each node
has an address and a size, representing an **Extent** within the
document's address space. A single **root node** spans the document's
entire address space, and other nodes can be created under it to
outline the structure of the data within the document.

A node's children must be positioned at monotonically increasing
addresses, but may overlap with each other either partially or
entirely. The children must be completely contained within the size of
their parent. Node **properties** can be set to control how a node's
title is displayed, how its children are displayed, and how any data
that belongs to it is displayed.

Parts of a node's extent that overlap with its children are
**shadowed**; that data does not belong to the parent node. If a node
has children, the children do not need to contiguously cover the
entire extent of the parent. If there are gaps in addresses between a
node's children, the data within those extents is not shadowed; it is
considered to belong to the parent node and will be displayed
according to the parent node's properties.

.. note::

   Children may overlap with each other. This may cause data to get
   repeated as it is displayed once for each overlapping child, and
   can appear as if the output is backtracking upon itself. This is
   intended behavior and allows the same data to be displayed under
   multiple different representations, which may be useful when
   analyzing polyglot files or exploit payloads.

Title Display
~~~~~~~~~~~~~

.. image:: ../screenshotter/structure_title_display.png
  :alt: A screenshot illustrating the three title display modes.

Inline
  Title is displayed on the same line as the first line of
  content. This is ideal when there are many small fields within a
  structure where the data will comfortably fit on the same line as
  the title and the vertical space can be saved.

Minor
  Title is displayed on a separate line before the content, but with
  no additional whitespace. Useful for sub-structures and longer
  fields that won't fit on a single line.

Major
  Title is displayed on a separate line before the content, and
  includes additional blank lines before the title and after the end
  of the content to give more visual separation. Best for top-level
  structures or particularly important fields that need to stand out
  from their surroundings.

Children Display
~~~~~~~~~~~~~~~~

Hidden
  Neither titles nor content are rendered for children, but their
  presence still shadows data in the parent node and prevents it from
  being displayed.

  .. note::

     Apparently not implemented yet?

Full
  All children are displayed as normal, including titles, and their
  presence affects data shadowing.


Summary
  The node's entire tree of children are displayed on a single line
  using a special JSON-style syntax. Non-leaf nodes do not display any
  data, even if it is not shadowed. This is intended for displaying
  small structures with two or three fields and hiding padding.

  .. note::

     The Summary children display mode is under development and known to
     be buggy. It is hidden behind a feature gate by default. TODO:
     actually feature gate it and describe here how to enable it.

Content Display
~~~~~~~~~~~~~~~

.. image:: ../screenshotter/structure_content_display.png
  :alt: A screenshot illustrating the three content display modes.

Hidden
  Data belonging to this node is skipped completely. This is most
  useful for hiding padding data between defined children such as
  archive members aligned to specific addresses or partitions within a
  flash dump. Note that in the image above, because the root node's
  content display is Hidden, the data from ``0x0-0x10`` and
  ``0x20-0x30`` is skipped.

Hexdump
  Data is displayed in classic hex editor style, as 16 columns of
  bytes with spaces between them and an ascii representation on the
  side.

  .. note::

     Functionality is implemented internally for displaying different
     amounts of Hexdump data per line, but is unfinished. Please file
     an issue if this is functionality you would find especially
     useful and I will prioritize it!

Hexstring
  Data is displayed on a single line as a continuous string of
  hexadecimal bytes. Currently limited to 16 bytes.

