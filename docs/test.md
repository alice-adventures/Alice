---
hide:
  - footer
tags:
  - Ada
  - Example
  - Bash
---

--8<-- "docs/snippets/alice-header.md"

# MKDOCS & MATERIAL <br>THEME TEST
*Adventures for Learning and Inspiring Coding Excellence*

This file contains tests for `mkdocs` and `material` theme, with particular
instructions to manage images and publications with versions.

## Formatting

### Basic Markdown

Text in __bold__ and _italic_.


```md title="character"
Text in __bold__ and _italic_.
```

### Critic Markup

Using [Critic] markup, this is a {--deleted--} {++added++} text, done in a
{~~one~>a single~~} operation. {==Highlight==} also helps.

{==

Formatting applied to a block of text. Several lines highlighted using the same
block of contents, useful to highlight important paragraphs over the rest of
paragraphs.

==}

### Keys

Using the [keys] extension one ca write keyboard commands:

```md title="keys"
Press ++ctrl+alt+"C"++ to exit.
```

Press ++ctrl+alt+"C"++ to exit.

### Mathjax

One can write maths like for $X = \{x_1, x_2, \ldots x_N\}$ with $\bar{X} = 1/N
\sum_{1}^{N}x_i$, variance is defined by

$$
  \sigma^2 = \frac{1}{N-1} \sum_{i=1}^{N} (x_i - \bar{X})^2
$$

```md title="mathjax"
One can write maths like for $X = \{x_1, x_2, \ldots x_N\}$ with
$\bar{X} = 1/N \sum_{1}^{N}x_i$, variance is defined by

$$
  \sigma^2 = \frac{1}{N-1} \sum_{i=1}^{N} (x_i - \bar{X})^2
$$
```

### Footnotes

```md title="footnote"
[^1]: Lorem Ipsum dolor sit amet, consectetur adipiscing elit.
```

Now jump to the footnote[^1] from this position.

And now jump to [Figure 1](#Figure-1)

## Block

### Admonitions

#### Types

!!! Note "Note with custom title"
    Note

!!! Abstract ""
    This Abstract has no title

??? Info
    Collapsible Info

???+ Tip
    Collapsible Tip initially expanded

!!! Success
    Success

!!! Question
    Question

!!! Warning
    Warning

!!! Failure
    Failure

!!! Danger
    Danger

!!! Bug
    Bug

!!! Example
    Example

!!! Quote
    Quote

#### Inline admonitions

=== "Inline"
    !!! Example inline
        Example admonition put inline of another Quote admonition.

    !!! Quote
        This is a regular Quote admonition.

        ```
        !!! Example inline
            Example admonition put inline of another Quote admonition.

        !!! Quote
            This is a regular Quote admonition.
        ```

=== "Inline end"
    !!! Example inline end
        Example admonition put inline of another Quote admonition.

    !!! Quote
        This is a regular Quote admonition.

        ```
        !!! Example inline end
            Example admonition put inline of another Quote admonition.

        !!! Quote
            This is a regular Quote admonition.
        ```

### Code

#### Basic block


```ada title="package"
   package Hello_World is
      procedure Say_Hello;
   end Hello_World;
```

#### Code with line numbers

```ada linenums="1" title="with lines"
   package Hello_World is
      procedure Say_Hello;
   end Hello_World;
```

#### Code with line numbers and title

```ada linenums="1" title="Nice example"
   package Hello_World is
      procedure Say_Hello;
   end Hello_World;
```

#### Code with line numbers, title and annotation

```ada linenums="1" title="Nice example"
   package Hello_World is
      procedure Say_Hello;  -- (1)!
   end Hello_World;
```

1. This is the classic procedure to write `Text_IO.Put_Line ("Hello, world");`

#### Code with line numbers, title, annotation and line highlighting

```ada linenums="1" title="Nice example" hl_lines="1 2"
   package Hello_World is
      procedure Say_Hello;
   end Hello_World;  -- (1)!
```

1. This line defines the end of the `Hello_World` package.

#### Code including an external file

!!! Note inline end
    Note that line 14 (highlighted) clearly shows _font ligatures_ enabled.

```js linenums="1" title="Javascript to enable MathJax" hl_lines="14"
--8<-- "docs/javascripts/mathjax.js"
```


## Images

### Complete example

![Alice_Adventures-top-right](img/Alice_Adventures-top-right-dark.png#only-dark){ width="51.77%" }
![Alice_Adventures-top-right](img/Alice_Adventures-top-right-light.png#only-light){ width="51.77%" }
<br><a name="Figure-1">__Figure 1__</a> - _Click the image in dark and light mode_
{ .figure }

### Without slide show

![Alice_Adventures-top-right](img/Alice_Adventures-top-right-dark.png#only-dark){ width="51.77%" .off-glb }
![Alice_Adventures-top-right](img/Alice_Adventures-top-right-light.png#only-light){ width="51.77%" .off-glb }
<br><a name="Figure-2">__Figure 2__</a> - _Do not click the image_
{ .figure }

<!-- REFERENCES -->
[keys]: https://facelessuser.github.io/pymdown-extensions/extensions/keys/
[Critic]: https://facelessuser.github.io/pymdown-extensions/extensions/critic/

<!-- Footnotes -->
[^1]: Lorem Ipsum dolor sit amet, consectetur adipiscing elit.
