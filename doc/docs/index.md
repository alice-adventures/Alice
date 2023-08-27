---
hide:
  - footer
tags:
  - Ada
  - Example
  - Bash
---

![Alice](Alice_Adventures-top-right-blue.png#only-light){ .alice align=right .off-glb }
![Alice](Alice_Adventures-top-right-white.png#only-dark){ .alice align=right .off-glb }

# WELCOME TO ALICE DOCUMENTATION TEST
*Adventures for Learning and Inspiring Coding Excellence*

<br>

## Introduction

This document is a test page of all `mkdocs` possibilities. As a short ref,
remember that `mkdocs` allows you to:

* `mkdocs new [dir-name]` - Create a new project.
* `mkdocs serve` - Start the live-reloading docs server.
* `mkdocs build` - Build the documentation site.
* `mkdocs -h` - Print help message and exit.

### Figures

Inclusion of figures require

```markdown
![Repositories](repositories-light.png#only-light){ width="70%" }
![Repositories](repositories-dark.png#only-dark){ width="70%" }
<br>**Figure 4** - *Repositories*
{ .figure }
```

Images for dark theme are prepared as follows:

  1. Export image from excalidraw in dark mode with no background.
  2. Run the following command:

  ```bash
  convert -background {--'#22252a'--} '#2e303e' -flatten input.png output.png
  ```

The result is shown in **Figure 1**.

![Repositories](repositories-light.png#only-light){ width="70%" }
![Repositories](repositories-dark.png#only-dark){ width="70%" }
<br>**Figure 1** - *Repositories*
{ .figure }

### Admonitions

#### Types

!!! Note "With custom title"
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

```markdown
  !!! Info inline end "Comment here"
      This is the package specification

  ```ada title="package.ads"
  package The_Package is
    pragma pure;
  end The_Package;

  ```
```

Produces:

!!! Info inline end "Comment here"
    This is the package specification

```ada title="package.ads"
package The_Package is
  pragma pure;
end The_Package;

```

### Code blocks

```ada title="My first program" linenums="1"
procedure Main is
begin
   null; -- (1)!
end Main;
```

1. This is a note

## Ada example

```ada title="Counting to 42" linenums="16"
with Ada.Text_IO;

package Hello_World is

   type task Interference is new Problem_Interface;

   procedure Hello_World is
      N : Natural := 42;       -- (1)!
   begin
      Ada.Text_IO.Put_Line ("Hello, world!");
   end Hello_World;

end Hello_World;
```

1. `#!ada Natural` is a prefefined type in the range \(\Sigma x_i = N\).

The above line has been stylized.


Vinci in nostris illos sunt supersunt. Humo tenenti, repens pectore ambrosiam
pulsant dicentem guttur bella. Beati oraque, increpor aequore in repulsa licet,
ponunt litus, cum **deinde** prioribus frigus causam.


![Project_Euler-screenshot](Project_Euler-screenshot-01.png)
<br>**Figure 4** - *Project_Euler screenshot*
{ .figure }



## Bash example

```bash title="git clone"
cd docs
git pull --all --tags --force  # (1)!
```

1. Tags can change on the remote site.

Now bash:

```sh
#!python hl lines="1 3"
ls -la
tree /
ls -l /
```

## Project layout

    mkdocs.yml    # The configuration file.
    docs/
        index.md  # The documentation homepage.
        ...       # Other markdown pages, images and other files.

## Tabbed Contents

=== "Specification"
    ```ada
    package Hellow_World is
       procedure Hello_World;
    end Hellow_World;
    ```

=== "Body"
    ```ada
    with Text_IO;
    package body Hellow_World is
       procedure Hello_World is
          Text_IO.Put_Line ("Hello, world");
       end Hello_World;
    end Hellow_World;
    ```
