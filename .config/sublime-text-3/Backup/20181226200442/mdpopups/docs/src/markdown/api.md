## Dependencies

Your plugin must include the Package Control dependencies listed below. Please read about Package Control's [dependencies][pc-dependencies] to learn more.

```js
{
    "*": {
        ">=3124": [
            "pygments",
            "python-markdown",
            "mdpopups",
            "python-jinja2",
            "markupsafe"
        ]
    }
}
```

## Markdown Support

MdPopups uses [Python Markdown][pymd] to parse Markdown and transform it into a popup or phantom (HTML embedded in your file view).  The Markdown environment supports basic Markdown features, but also includes a number of specialty extensions to enhance the environment.  To keep the experience standardized for plugin use, tweaking the Markdown settings is not allowed except for a few things like enabling/disabling `nl2br` etc.

MdPopups includes the following Python Markdown extensions, but some of the features may not be used due to the limitations of Sublime's `minihtml`.

- [`attr_list`][attr_list] allows you to add HTML attributes to block and inline elements easily.
- [`nl2br`][nl2br] turns new lines into `#!html <br>` tags.
- [`def_list`][def_list] adds support for definition lists.
- [`admonition`][admonition] provides admonition blocks.

MdPopups also includes a couple of 3rd party extensions (some of which have been modified to work better in the Sublime Text environment).

- [`superfences`][superfences] provides support for nested fenced blocks.
- [`betterem`][betterem] is an extension that aims to improve emphasis support in Python Markdown. MdPopups leaves it configured in its default state where underscores are handled intelligently: `_handled_intelligently_` --> _handled_intelligently_ and asterisks can be used to do mid word emphasis: `em*pha*sis` --> em*pha*sis.
- [`magiclink`][magiclink] auto links HTML links.
- [`inlinehilite`][inlinehilite] allows for inline code highlighting: `` `#!python import module` `` --> `#!python import module`.
- [`extrarawhtml`][extrarawhtml] allows you to add `markdown="1"` to block HTML elements to allow content under them to be parsed with Python markdown (inline tags should already have their content parsed).  All this module does is expose this specific functionality from the [Python Markdown's Extra extension](https://pythonhosted.org/Markdown/extensions/extra.html#nested-markdown-inside-html-blocks) as this functionality could not be enabled without including all of the `Extra` extensions other features.  You can read the Python Markdown's Extra extension documentation to learn more about this feature.
- [`highlight`][highlight] controls and configures the highlighting of code blocks.

## Styling

Popups and phantoms are styled with CSS that is fed through the Jinja2 template engine. A default CSS is provided that styles commonly used elements. Plugins can provide CSS to add additional styling for plugin specific purposes. See [CSS Styling](./styling.md) to learn more about the template engine and general styling info.

It is advised to use the `wrapper_class` option of the `show_popup`, `update_popup`, and `add_phantom` commands to wrap your plugin content in a div with a unique, plugin specific class.  This way plugins can inject CSS to style their specific elements via `#!css .mdpopups .myplugin-wrapper .myclass {}` or simply `#!css .myplugin-wrapper .myclass {}`.

Also check out the included Python Markdown [`attr_list` extension syntax](https://pythonhosted.org/Markdown/extensions/attr_list.html). This is a good extension for applying classes directly to elements within Markdown format. Sometimes it can be difficult to target certain kinds of block elements, so if all else fails, you can insert raw HTML for specific elements into your Markdown and apply classes directly to them.

## API Usage

MdPopups provides a number of accessible functions.

### Version

`(int,) mdpopups.version`
: 
    Returns the version of the MdPopups library.  Returns a tuple of integers which represents the major, minor, and patch version.

### Show Popup

`mdpopups.show_popup`
: 
    Accepts Markdown and creates a Sublime popup.  By default, the built-in Sublime syntax highlighter will be used for code highlighting.

    Parameter              | Type                | Default      | Description
    ---------------------- | ------------------- | ------------ | -----------
    `view`                 | `#!py sublime.View` |              | A Sublime Text view object.
    `content`              | `#!py str`          |              | Markdown/HTML content for the popup.
    `md`                   | `#!py bool`         | `#!py True`  | Defines whether the content is Markdown and needs to be converted.
    `css`                  | `#!py str`          | `#!py None`  | Additional CSS that will be injected.
    `flags`                | `#!py int`          | `#!py 0`     | Flags to pass down to the Sublime Text `view.show_popup` call.
    `location`             | `#!py int`          | `#!py -1`    | Location to show popup in view.  -1 means to show right under the first cursor.
    `max_width`            | `#!py int`          | `#!py 320`   | Maximum width of the popup.
    `max_height`           | `#!py int`          | `#!py 240`   | Maximum height of the popup.
    `on_navigate`          | `#!py def fn()`     | `#!py None`  | Callback that receives one variable `href`.
    `on_hide`              | `#!py def fn()`     | `#!py None`  | Callback for when the popup is hidden.
    `wrapper_class`        | `#!py str`          | `#!py None`  | A string containing the class name you wish wrap your content in.  A `div` will be created with the given class.
    `template_vars`        | `#!py dict`         | `#!py None`  | A dictionary containing template vars.  These can be used in either the CSS or the HTML/Markdown content. These vars are found under the object `plugin`.
    `template_env_options` | `#!py dict`         | `#!py None`  | A dictionary containing options for the Jinja2 template environment. This **only** applies to the **HTML/Markdown** content.
    `nl2br`                | `#!py bool`         | `#!py True`  | Determines whether the newline to `#!html <br>` Python Markdown extension is enabled or not.
    `allow_code_wrap`      | `#!py bool`         | `#!py False` | Do not convert all the spaces in code blocks to `&nbsp;` so that wrapping can occur.

### Update Popup

`mdpopups.update_popup`
: 
    Updates the current existing popup.

    Parameter              | Type                | Default      | Description
    ---------------------- | ------------------- | ------------ | -----------
    `view`                 | `#!py sublime.View` |              | A Sublime Text view object.
    `content`              | `#!py str`          |              | Markdown/HTML content for the popup.
    `md`                   | `#!py bool`         | `#!py True`  | Defines whether the content is Markdown and needs to be converted.
    `css`                  | `#!py str`          | `#!py None`  | Additional CSS that will be injected.
    `wrapper_class`        | `#!py str`          | `#!py None`  | A string containing the class name you wish wrap your content in.  A `div` will be created with the given class.
    `template_vars`        | `#!py dict`         | `#!py None`  | A dictionary containing template vars.  These can be used in either the CSS or the HTML/Markdown content. These vars are found under the object `plugin`.
    `template_env_options` | `#!py dict`         | `#!py None`  | A dictionary containing options for the Jinja2 template environment. This **only** applies to the **HTML/Markdown** content.
    `nl2br`                | `#!py bool`         | `#!py True`  | Determines whether the newline to `#!html <br>` Python Markdown extension is enabled or not.
    `allow_code_wrap`      | `#!py bool`         | `#!py False` | Do not convert all the spaces in code blocks to `&nbsp;` so that wrapping can occur.

### Hide Popup

`mdpopups.hide_popup`
: 
    Hides the current popup.  Included for convenience and consistency.

    Parameter | Type                | Default | Description
    --------- | ------------------- | ------- | -----------
    `view`    | `#!py sublime.View` |         | A Sublime Text view object.


### Is Popup Visible

`bool mdpopups.is_popup_visible`
: 
    Checks if popup is visible in the view. Included for convenience and consistency.

    Parameter | Type                | Default | Description
    --------- | ------------------- | ------- | -----------
    `view`    | `#!py sublime.View` |         | A Sublime Text view object.

### Add Phantom

`int mdpopups.add_phantom`
: 
    Adds a phantom to the view and returns the phantom id as an integer. By default, the built-in Sublime syntax highlighter will be used for code highlighting. 

    Parameter              | Type                  | Default      | Description
    ---------------------- | --------------------- | ------------ | -----------
    `view`                 | `#!py sublime.View`   |              | A Sublime Text view object.
    `key`                  | `#!py str`            |              | A key that is associated with the given phantom.  Multiple phantoms can share the same key, but each phantom will have its own id.
    `region`               | `#!py sublime.Region` |              | Region in the view where the phantom should be inserted.
    `content`              | `#!py str`            |              | Markdown/HTML content for the phantom.
    `layout`               | `#!py int`            |              | How the HTML content should be inserted.  Acceptable values are: `sublime.LAYOUT_INLINE`, `sublime.LAYOUT_BLOCK`, and `sublime.LAYOUT_BELOW`.
    `md`                   | `#!py bool`           | `#!py True`  | Defines whether the content is Markdown and needs to be converted.
    `css`                  | `#!py str`            | `#!py None`  | Additional CSS that will be injected.
    `on_navigate`          | `#!py def fn()`       | `#!py None`  | Callback that receives one variable `href`.
    `wrapper_class`        | `#!py str`            | `#!py None`  | A string containing the class name you wish wrap your content in.  A `div` will be created with the given class.
    `template_vars`        | `#!py dict`           | `#!py None`  | A dictionary containing template vars.  These can be used in either the CSS or the HTML/Markdown content.A dictionary containing template vars.  These can be used in either the CSS or the HTML/Markdown content. These vars are found under the object `plugin`.
    `template_env_options` | `#!py dict`           | `#!py None`  | A dictionary containing options for the Jinja2 template environment. This **only** applies to the **HTML/Markdown** content. Content plugin vars are found under the object: `plugin`.A dictionary containing options for the Jinja2 template environment. This **only** applies to the **HTML/Markdown** content.
    `nl2br`                | `#!py bool`           | `#!py True`  | Determines whether the newline to `#!html <br>` Python Markdown extension is enabled or not.
    `allow_code_wrap`      | `#!py bool`           | `#!py False` | Do not convert all the spaces in code blocks to `&nbsp;` so that wrapping can occur.

### Erase Phantoms

`mdpopups.erase_phantoms`
: 
    Erase all phantoms associated with the given key. Included for convenience and consistency.

    Parameter | Type                | Default | Description
    --------- | ------------------- | ------- | -----------
    `view`    | `#!py sublime.View` |         | A Sublime Text view object.
    `key`     | `#!py str`          |         | A key that is associated with phantoms.  Multiple phantoms can share the same key, but each phantom will have its own id.

### Erase Phantom by ID

`mdpopups.erase_phantom_by_id`
: 
    Erase a single phantom by its id.  Included for convenience and consistency.

    Parameter   | Type                | Default | Description
    ----------- | ------------------- | ------- | -----------
    `view`      | `#!py sublime.View` |         | A Sublime Text view object.
    `pid`       | `#!py str`          |         | The id associated with a single phantom.  Multiple phantoms can share the same key, but each phantom will have its own id.

### Query Phantom

`[sublime.Region] mdpopups.query_phantom`
: 
    Query the location of a phantom by specifying its id.  A list of `sublime.Region`s will be returned.  If the phantom with the given id is not found, the region will be returned with positions of `(-1, -1)`.  Included for convenience and consistency.

    Parameter | Type                | Default | Description
    --------- | ------------------- | ------- | -----------
    `view`    | `#!py sublime.View` |         | A Sublime Text view object.
    `pid`     | `#!py int`          |         | The id associated with a single phantom.  Multiple phantoms can share the same key, but each phantom will have its own id.

### Query Phantoms

`[sublime.Region] mdpopups.query_phantoms`
: 
    Query the location of multiple phantoms by specifying their ids.  A list of `sublime.Region`s will be returned where each index corresponds to the index of ids that was passed in.  If a given phantom id is not found, that region will be returned with positions of `(-1, -1)`.  Included for convenience and consistency.

    Parameter | Type                | Default | Description
    --------- | ------------------- | ------- | -----------
    `view`    | `#!py sublime.View` |         | A Sublime Text view object.
    `pids`    | `#!py [int]`        |         | A list of ids associated with phantoms.  Multiple phantoms can share the same key, but each phantom will have its own id.

### Phantom Class

`mdpopups.Phantoms`
: 
    A phantom object for use with [`PhantomSet`](#phantomset-class).

    Parameter              | Type                  | Default      | Description
    ---------------------- | --------------------- | ------------ | -----------
    `region`               | `#!py sublime.Region` |              | Region in the view where the phantom should be inserted.
    `content`              | `#!py str`            |              | Markdown/HTML content for the phantom.
    `layout`               | `#!py int`            |              | How the HTML content should be inserted.  Acceptable values are: `sublime.LAYOUT_INLINE`, `sublime.LAYOUT_BLOCK`, and `sublime.LAYOUT_BELOW`.
    `md`                   | `#!py bool`           | `#!py True`  | Defines whether the content is Markdown and needs to be converted.
    `css`                  | `#!py str`            | `#!py None`  | Additional CSS that will be injected.
    `on_navigate`          | `#!py def fn()`       | `#!py None`  | Callback that receives one variable `href`.
    `wrapper_class`        | `#!py str`            | `#!py None`  | A string containing the class name you wish wrap your content in.  A `div` will be created with the given class.
    `template_vars`        | `#!py dict`           | `#!py None`  | A dictionary containing template vars.  These can be used in either the CSS or the HTML/Markdown content.A dictionary containing template vars.  These can be used in either the CSS or the HTML/Markdown content. These vars are found under the object `plugin`.
    `template_env_options` | `#!py dict`           | `#!py None`  | A dictionary containing options for the Jinja2 template environment. This **only** applies to the **HTML/Markdown** content. Content plugin vars are found under the object: `plugin`.A dictionary containing options for the Jinja2 template environment. This **only** applies to the **HTML/Markdown** content.
    `nl2br`                | `#!py bool`           | `#!py True`  | Determines whether the newline to `#!html <br>` Python Markdown extension is enabled or not.
    `allow_code_wrap`      | `#!py bool`           | `#!py False` | Do not convert all the spaces in code blocks to `&nbsp;` so that wrapping can occur.

    **Attributes**

    Attribute              | Type                  | Description
    ---------------------- | --------------------- | -----------
    `region`               | `#!py sublime.Region` | Region in the view where the phantom should be inserted.
    `content`              | `#!py str`            | Markdown/HTML content for the phantom.
    `layout`               | `#!py int`            | How the HTML content should be inserted.  Acceptable values are: `sublime.LAYOUT_INLINE`, `sublime.LAYOUT_BLOCK`, and `sublime.LAYOUT_BELOW`.
    `md`                   | `#!py bool`           | Defines whether the content is Markdown and needs to be converted.
    `css`                  | `#!py str`            | Additional CSS that will be injected.
    `on_navigate`          | `#!py def fn()`       | Callback that receives one variable `href`.
    `wrapper_class`        | `#!py str`            | A string containing the class name you wish wrap your content in.  A `div` will be created with the given class.
    `template_vars`        | `#!py dict`           | A dictionary containing template vars.  These can be used in either the CSS or the HTML/Markdown content.A dictionary containing template vars.  These can be used in either the CSS or the HTML/Markdown content. These vars are found under the object `plugin`.
    `template_env_options` | `#!py dict`           | A dictionary containing options for the Jinja2 template environment. This **only** applies to the **HTML/Markdown** content. Content plugin vars are found under the object: `plugin`.A dictionary containing options for the Jinja2 template environment. This **only** applies to the **HTML/Markdown** content.
    `nl2br`                | `#!py bool`           | Determines whether the newline to `#!html <br>` Python Markdown extension is enabled or not.
    `allow_code_wrap`      | `#!py bool`           | Do not convert all the spaces in code blocks to `&nbsp;` so that wrapping can occur.

### Phantom Set Class

`mdpopups.PhantomSet`
: 
    A class that allows you to update phantoms under the specified key.

    Parameter | Type                | Default | Description
    --------- | ------------------- | ------- | -----------
    `view`    | `#!py sublime.View` |         | A Sublime Text view object.
    `key`     | `#!py str`          |         | The key that should be associated with all related phantoms in the set.

    **Methods**

    `mdpopups.PhantomSet.update`
    : 
        Update all the phantoms in the set with the given phantom list.

        Parameter      | Type                                        | Default | Description
        -------------- | ------------------------------------------- | ------- | -----------
        `new_phantoms` | [`#!py [mdpopups.Phantom]`](#class-phantom) |         | A list of MdPopups phantoms. `sublime.Phantom` will be converted to `mdpopups.Phantom`.

### Clear Cache

`mdpopups.clear_cache`
: 
    Clears the CSS theme related caches.

### Markdown to HTML

`mdpopups.md2html`
: 
    Exposes the Markdown to HTML converter in case it is desired to parse only a section of markdown.  This works well for someone who wants to work directly in HTML, but might want to still have fragments of markdown that they would like to occasionally convert. By default, the built-in Sublime syntax highlighter will be used for code highlighting.

    Parameter              | Type                | Required | Default      | Description
    ---------------------- | ------------------- | -------- | ------------ | -----------
    `view`                 | `#!py sublime.View` | Yes      |              | Sublime text View object.
    `markup`               | `#!py string`       | Yes      |              | The markup code to be converted.
    `template_vars`        | `#!py dict`         | No       | `#!py None`  | A dictionary containing template vars.  These can be used in either the CSS or the HTML/Markdown content.A dictionary containing template vars.  These can be used in either the CSS or the HTML/Markdown content. These vars are found under the object `plugin`.
    `template_env_options` | `#!py dict`         | No       | `#!py None`  | A dictionary containing options for the Jinja2 template environment. This **only** applies to the **HTML/Markdown** content. Content plugin vars are found under the object: `plugin`.A dictionary containing options for the Jinja2 template environment. This **only** applies to the **HTML/Markdown** content.
    `nl2br`                | `#!py bool`         | No       | `#!py True`  | Determines whether the newline to `#!html <br>` Python Markdown extension is enabled or not.
    `allow_code_wrap`      | `#!py bool`         | No       | `#!py False` | Do not convert all the spaces in code blocks to `&nbsp;` so that wrapping can occur.

### Color Box

`string mdpopups.color_box`
: 
    Generates a color preview box image encoded in base 64 and formatted to be inserted right in your your Markdown or HTML code as an `img` tag.

    Parameter     | Type         | Default      | Description
    ------------- | ------------ | ------------ | -----------
    `colors`      | `#!py [str]` |              | A list of color strings formatted as `#RRGGBBAA` where `R` is the red channel, `G` is the green channel, `B` is the blue channel, and `A` is the alpha channel.
    `border`      | `#!py str`   |              | The color for the color box border.  Input is a RGB color formatted as `#RRGGBB`.
    `border2`     | `#!py str`   | `#!py None`  | The optional secondary border color.  This is great if you are going to have it on a light and dark backgrounds.  You can use a double border so the color stands out regardless of the background.  Input is a RGB color formatted as `#RRGGBB`.
    `height`      | `#!py int`   | `#!py 32`    | Height of color box.
    `width`       | `#!py int`   | `#!py 32`    | Width of color box.
    `border_size` | `#!py int`   | `#!py 1`     | Width of the color box border.  If using `border2`, the value should be set to at least 2 to see both colors.
    `check_size`  | `#!py int`   | `#!py 4`     | Size of checkered box squares used for the background of transparent colors.
    `max_colors`  | `#!py int`   | `#!py 5`     | Max number of colors that will be evaluated in the `colors` parameter.  Multiple colors are used to to create palette boxes showing multiple colors lined up horizontally.
    `alpha`       | `#!py bool`  | `#!py False` | Will create color box images with a real alpha channel instead of simulating one with a checkered background.
    `border_map`  | `#!py int`   | `#!py 0xF`   | A mapping of which borders to show.  Where `0x1` is `TOP`, `0x2` is `LEFT`, `0x4` is `BOTTOM`, `0x8` is `RIGHT`.  Map flags can be accessed via `mdpopups.colorbox.TOP` etc.

### Color Box Raw

`bytes mdpopups.color_box`
: 
    Generates a color preview box image and returns the raw byte string of the image.

    Parameter     | Type         | Default      | Description
    ------------- | ------------ | ------------ | -----------
    `colors`      | `#!py [str]` |              | A list of color strings formatted as `#RRGGBBAA` where `R` is the red channel, `G` is the green channel, `B` is the blue channel, and `A` is the alpha channel.
    `border`      | `#!py str`   |              | The color for the color box border.  Input is a RGB color formatted as `#RRGGBB`.
    `border2`     | `#!py str`   | `#!py None`  | The optional secondary border color.  This is great if you are going to have it on a light and dark backgrounds.  You can use a double border so the color stands out regardless of the background.  Input is a RGB color formatted as `#RRGGBB`.
    `height`      | `#!py int`   | `#!py 32`    | Height of color box.
    `width`       | `#!py int`   | `#!py 32`    | Width of color box.
    `border_size` | `#!py int`   | `#!py 1`     | Width of the color box border.  If using `border2`, the value should be set to at least 2 to see both colors.
    `check_size`  | `#!py int`   | `#!py 4`     | Size of checkered box squares used for the background of transparent colors.
    `max_colors`  | `#!py int`   | `#!py 5`     | Max number of colors that will be evaluated in the `colors` parameter.  Multiple colors are used to to create palette boxes showing multiple colors lined up horizontally.
    `alpha`       | `#!py bool`  | `#!py False` | Will create color box images with a real alpha channel instead of simulating one with a checkered background.
    `border_map`  | `#!py int`   | `#!py 0xF`   | A mapping of which borders to show.  Where `0x1` is `TOP`, `0x2` is `LEFT`, `0x4` is `BOTTOM`, `0x8` is `RIGHT`.  Map flags can be accessed via `mdpopups.colorbox.TOP` etc.

### Tint

`string mdpopups.tint`
: 
    Takes a either a path to an PNG or a byte string of a PNG and tints it with a specific color and returns a string containing the base 64 encoded PNG in a HTML element.

    Parameter | Type             | Default     | Description
    --------- | ---------------- | ----------- | -----------
    `img`     | `#!py str/bytes` |             | Either a string in the form `Packages/Package/resource.png` or a byte string of a PNG image.
    `color`   | `#!py str`       |             | A string in the form of `#RRGGBB` or `#RRGGBBAA` (alpha layer will be stripped and ignored and is only allowed to make it easy to pass in colors from a color scheme).
    `opacity` | `#!py int`       | `#!py 255`  | An integer value between 0 - 255 that specifies the opacity of the tint.
    `height`  | `#!py int`       | `#!py None` | Height that should be specified in the return HTML element.
    `width`   | `#!py int`       | `#!py None` | Width that should be specified in the return HTML element.

### Tint Raw

`bytes mdpopups.tint_raw`
: 
    Takes a either a path to an PNG or a byte string of a PNG and tints it with a specific color and returns a byte string of the modified PNG.

    Parameter | Type             | Default    | Description
    --------- | ---------------- | ---------- | -----------
    `img`     | `#!py str/bytes` |            | Either a string in the form `Packages/Package/resource.png` or a byte string of a PNG image.
    `color`   | `#!py str`       |            | A string in the form of `#RRGGBB` or `#RRGGBBAA` (alpha layer will be stripped and ignored and is only allowed to make it easy to pass in colors from a color scheme).
    `opacity` | `#!py int`       | `#!py 255` | An integer value between 0 - 255 that specifies the opacity of the tint.

### Scope to Style

`dict mdpopups.scope2style`
: 
    Takes a sublime scope (complexity doesn't matter), and guesses the style that would be applied.  While there may be untested corner cases with complex scopes where it fails, in general, it is usually accurate.  The returned dictionary is in the form:

    ```py
    {
        # Colors will be None if not found,
        # though usually, even if the scope has no color
        # it will return the overall theme foreground.
        #
        # Background might be None if using `explicit_background`
        # as it only returns a background if that style specifically
        # defines a background.
        "color": "#RRGGBB",
        "background": "#RRGGBB",
        # Style will usually be either 'bold', 'italic'.
        # Multiple styles may be returned 'bold italic' or an empty string ''.
        "style": 'bold italic'
    }
    ```

    Parameter             | Type                | Default      | Description
    --------------------- | ------------------- | ------------ | -----------
    `view`                | `#!py sublime.View` |              | Sublime text View object so that the correct color scheme will be searched.
    `scope`               | `#!py string`       |              | The scope to search for.
    `selected`            | `#!py bool`         | `#!py False` | Whether this scope is in a selected state (selected text).
    `explicit_background` | `#!py bool`         | `#!py False` | Only return a background if one is explicitly defined in the color scheme.

### Syntax Highlight

`mdpopups.syntax_highlight`
: 
    Allows for syntax highlighting outside the Markdown environment.  You can just feed it code directly and give it the language of your choice, and you will be returned a block of HTML that has been syntax highlighted. By default, the built-in Sublime syntax highlighter will be used for code highlighting.

    Parameter         | Type                | Default      | Description
    ----------------- | ------------------- | ------------ | -----------
    `view`            | `#!py sublime.View` |              | Sublime text View object.
    `src`             | `#!py str`          |              | The source code to be converted.  No fence tokes are needed (` ``` `).
    `language`        | `#!py str`          | `#!py None`  | Specifies the language to highlight as.
    `inline`          | `#!py bool`         | `#!py False` | Will return the code formatted for inline display.
    `allow_code_wrap` | `#!py bool`         | `#!py False` | Do not convert all the spaces in code blocks to `&nbsp;` so that wrapping can occur.

### Get Language From View

`mdpopups.get_language_from_view`
: 
    Allows a user to extract the equivalent language specifier for `mdpopups.syntax_highlight` from a view.  If the language cannot be determined, `None` will be returned.

    Parameter | Type                | Default | Description
    --------- | ------------------- | ------- | -----------
    `view`    | `#!py sublime.View` |         | Sublime text View object.

--8<-- "refs.md"
