# User Guide {: .doctitle}
Using and configuring Sublime Markdown Popups.

---

## Dependencies
Your plugin must include the following Package Control dependencies:

```js
{
    "*": {
        ">=3080": [
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
MdPopups uses [Python Markdown](https://pythonhosted.org/Markdown/) to parse Markdown and transform it into a tooltip or a phantom (HTML embedded in your file view).  The Markdown environment supports basic Markdown features, but also includes a number of specialty extensions to enhance the environment.  To keep the experience standardized for plugin use, tweaking the Markdown settings is not allowed except for `nl2br` as it is not critical and can actually get in the way of formatting the Markdown if not desired.

MdPopups includes the following Python Markdown extensions:

- [attr_list](https://pythonhosted.org/Markdown/extensions/attr_list.html) allows you to add HTML attributes to block and inline elements easily.
- [nl2br](https://pythonhosted.org/Markdown/extensions/nl2br.html) turns new lines int `#!html <br>` tags.
- [def_list](https://pythonhosted.org/Markdown/extensions/definition_lists.html) adds support for definition lists.
- [admonition](https://pythonhosted.org/Markdown/extensions/admonition.html) provides admonition blocks.
- [codehilite](https://pythonhosted.org/Markdown/extensions/code_hilite.html) provides syntax highlighted blocks.

MdPopups also includes a couple of 3rd party extensions (some of which have been modified to work better in the Sublime Text environment).

- [superfences](http://facelessuser.github.io/pymdown-extensions/extensions/superfences/) provides support for nested fenced blocks. UML support is disabled.
- [betterem](http://facelessuser.github.io/pymdown-extensions/extensions/betterem/) is extension that aims to improve emphasis support in Python Markdown. MdPopups leaves it configured in its default state where underscores are handled intelligently: `_handled_intelligently_` --> _handled_intelligently_.  Asterisks can be used to do mid word emphasis: `em*pha*sis` --> em*pha*sis.
- [magiclink](http://facelessuser.github.io/pymdown-extensions/extensions/magiclink/) auto links HTML links.
- [inlinehilite](http://facelessuser.github.io/pymdown-extensions/extensions/inlinehilite/) allows for inline code highlighting: `` `#!python import module` `` --> `#!python import module`.
- [extrarawhtml](http://facelessuser.github.io/pymdown-extensions/extensions/extrarawhtml/) allows you to add `markdown="1"` to block html elements to allow content under them to be parsed with Python markdown (inline tags should already have their content parsed).  All this module does is expose this specific functionality from the [Python Markdown’s Extra extension](https://pythonhosted.org/Markdown/extensions/extra.html#nested-markdown-inside-html-blocks) as this functionality could not be enabled without including all of the `Extra` extensions other features.  You can read the Python Markdown's Extra extension documentation to learn more about this feature.

!!! hint "New 1.10.0"
    `extrarawhtml` was added.

!!! hint "New 1.9.0"
    `nl2br` can be turned off via the `nl2br` parameter in `show_popup`, `add_phantom`, `update_popup`, `md2html`, and `Phantom`.

## API Usage
MdPopups provides a number of accessible functions.

!!! caution "Developer Guidelines"
    Plugin developers should not try to override the style of existing base classes and elements with plugin injection, but they should use custom plugin classes so that only the specific special elements that must be handled uniquely for the plugin get targeted.  You should use very unique class names (preferably with the plugin's name as part of the class). This way a user can target and override your class styling if desired. There are a couple of ways to approach this.

    - For Sublime Text 3119+, it is advised to use the `wrapper_class` option of the `show_popup`, `update_popup`, and `add_phantom` commands to wrap your content in a div with the provided class.  That way the developer can provide CSS to style their specific elements via `#!css .mdpopups .myplugin-wrapper .myclass {}` or simply `#!css .myplugin-wrapper .myclass {}`. This is one of the easiest ways, but it is for 3119+ only.

    - For Sublime Text <3119, when injecting your own CSS classes from a plugin, wrapper classes won't work as Sublime didn't add CSS support for parent child classes until later. In this case, it is recommend you namespace your classes by appending the plugin name as a prefix so it can be targeted like this: `#!css .myplugin-myclass {}`.  This will give your elements very unique classes that the user can target and override if they choose.

    - To add classes to inline and some block markdown elements you can use the Python Markdown [attr_list extension syntax](https://pythonhosted.org/Markdown/extensions/attr_list.html).  This will work on inline elements and a number of block elements (though sometimes it can be difficult to target certain kinds of block elements).  If all else fails, you can insert raw HTML into your markdown and apply classes directly to the element.

### version
(int,) mdpopups.version
: 
    Get the version of the MdPopups library.  Returns a tuple of integers which represents the major, minor, and patch version.

### show_popup
mdpopups.show_popup
: 
    Accepts Markdown and creates a Sublime popup tooltip.  By default, the Pygments syntax highlighter will be used for code highlighting.  Set [`mdpopups.use_sublime_highlighter`](#mdpopupsuse_sublime_highlighter) to `true` in your `Preferences.sublime-settings` file if you would like to use the Sublime syntax highlighter.

    | Parameter | Type | Required | Default | Description |
    | --------- | ---- | -------- | ------- | ----------- |
    | view | sublime.View | Yes | | A Sublime Text view object. |
    | content | string | Yes | | Markdown/HTML content to be used to create a tooltip. |
    | md | bool | No | True | Defines whether the content is Markdown and needs to be converterted. |
    | css | string | No | None | Additional CSS that will be injected. |
    | flags | int | No | 0 | Flags to pass down to the Sublime Text `view.show_popup` call. |
    | location | int | No | -1 | Location to show popup in view.  -1 means to show right under the first cursor. |
    | max_width | int | No | 320 | Maximum width of the popup. |
    | max_height | int | No | 240 | Maximum height of the popup. |
    | on_navigate | function | No | None | Callback that receives one variable `href`. |
    | on_hide | function | No | None | Callback for when the tooltip is hidden. |
    | wrapper_class | string | No | None | A string containing the class name you wish wrap your content in.  A `div` will be created with the given class. |
    | template_vars | dict | No | None | A dictionary containing template vars.  These can be used in either the CSS or the HTML/Markdown content. |
    | template_env_options | dict | No | None | A dictionary containing options for the Jinja2 template environment. This **only** applies to the **HTML/Markdown** content. Content plugin vars are found under the object: `plugin`. |
    | nl2br | bool | No | True | Determines whether the newline to br Python Markdown extension is enabled or not. |

    !!! hint "New 1.9.0"
        `wrapper_class`, `template_vars`, `template_env_options`, and `nl2br` option added in `1.9.0`.


### update_popup
mdpopups.update_popup
: 
    Updates the current existing popup.  Set [`mdpopups.use_sublime_highlighter`](#mdpopupsuse_sublime_highlighter) to `true` in your `Preferences.sublime-settings` file if you would like to use the Sublime syntax highlighter.

    | Parameter | Type | Required | Default | Description |
    | --------- | ---- | -------- | ------- | ----------- |
    | view | sublime.View | Yes | | A Sublime Text view object. |
    | content | string | Yes | | Markdown/HTML content to be used to create a tooltip. |
    | md | bool | No | True | Defines whether the content is Markdown and needs to be converterted. |
    | css | string | No | None | CSS text that should be used instead of loading a theme. |
    | wrapper_class | string | No | None | A string containing the class name you wish wrap your content in.  A `div` will be created with the given class. |
    | template_vars | dict | No | None | A dictionary containing template vars.  These can be used in either the CSS or the HTML/Markdown content. |
    | template_env_options | dict | No | None | A dictionary containing options for the Jinja2 template environment. This **only** applies to the **HTML/Markdown** content. Content plugin vars are found under the object: `plugin`. |
    | nl2br | bool | No | True | Determines whether the newline to br Python Markdown extension is enabled or not. |

    !!! hint "New 1.9.0"
        `wrapper_class`, `template_vars`, `template_env_options`, and `nl2br` option added in `1.9.0`.

### hide_popup
mdpopups.hide_popup
: 
    Hides the current popup.  Included for convenience and consistency.

    | Parameter | Type | Required | Default | Description |
    | --------- | ---- | -------- | ------- | ----------- |
    | view | sublime.View | Yes | | A Sublime Text view object. |


### is_popup_visible
bool mdpopups.is_popup_visible
: 
    Checks if popup is visible in the view. Included for convenience and consistency.

    | Parameter | Type | Required | Default | Description |
    | --------- | ---- | -------- | ------- | ----------- |
    | view | sublime.View | Yes | | A Sublime Text view object. |

    !!! hint "New 1.6.0"
        Feature added in `1.6.0`.

### add_phantom
int mdpopups.add_phantom
: 
    Adds a phantom (embedded HTML in the file view) and returns the phantom id.  Returns an integer.
    Accepts Markdown and creates a Sublime phantom (embedded HTML in the file view).  By default, the Pygments syntax highlighter will be used for code highlighting.  Set [`mdpopups.use_sublime_highlighter`](#mdpopupsuse_sublime_highlighter) to `true` in your `Preferences.sublime-settings` file if you would like to use the Sublime syntax highlighter.  On completion of the phantom, the function will return the phantom id which is an integer.

    | Parameter | Type | Required | Default | Description |
    | --------- | ---- | -------- | ------- | ----------- |
    | view | sublime.View | Yes | | A Sublime Text view object. |
    | key | string | Yes | | A key that is associated with the given phantom.  Multiple phantoms can share the same key, but each phantom will have its own id. |
    | region | sublime.Region | Yes | | Region in the view where the phantom should be inserted. |
    | content | string | Yes | | Markdown/HTML content to be used to create a phantom. |
    | layout | int | Yes | | How the HTML content should be inserted.  Acceptable values are: `sublime.LAYOUT_INLINE`, `sublime.LAYOUT_BLOCK`, and `sublime.LAYOUT_BELOW`. |
    | md | bool | No | True | Defines whether the content is Markdown and needs to be converterted. |
    | css | string | No | None | Additional CSS that will be injected. |
    | on_navigate | function | No | None | Callback that receives one variable `href`. |
    | wrapper_class | string | No | None | A string containing the class name you wish wrap your content in.  A `div` will be created with the given class. |
    | template_vars | dict | No | None | A dictionary containing template vars.  These can be used in either the CSS or the HTML/Markdown content. |
    | template_env_options | dict | No | None | A dictionary containing options for the Jinja2 template environment. This **only** applies to the **HTML/Markdown** content. Content plugin vars are found under the object: `plugin`. |
    | nl2br | bool | No | True | Determines whether the newline to br Python Markdown extension is enabled or not. |

    !!! hint "New 1.9.0"
        `wrapper_class`, `template_vars`, `template_env_options`, and `nl2br` option added in `1.9.0`.

    !!! hint "New 1.6.0"
        Feature added in `1.6.0`.


### erase_phantoms
mdpopups.erase_phantoms
: 
    Erase all phantoms associated to the given key.  Included for convenience and consistency.

    | Parameter | Type | Required | Default | Description |
    | --------- | ---- | -------- | ------- | ----------- |
    | view | sublime.View | Yes | | A Sublime Text view object. |
    | key | string | Yes | | A key that is associated with phantoms.  Multiple phantoms can share the same key, but each phantom will have its own id. |

    !!! hint "New 1.6.0"
        Feature added in `1.6.0`.

### erase_phantom_by_id
mdpopups.erase_phantom_by_id
: 
    Erase a single phantom by passing its id.  Included for convenience and consistency.

    | Parameter | Type | Required | Default | Description |
    | --------- | ---- | -------- | ------- | ----------- |
    | view | sublime.View | Yes | | A Sublime Text view object. |
    | pid | string | Yes | | The id associated with a single phantom.  Multiple phantoms can share the same key, but each phantom will have its own id. |

    !!! hint "New 1.6.0"
        Feature dded in `1.6.0`.

### query_phantom
[sublime.Region] mdpopups.query_phantom
: 
    Query the location of a phantom by specifying its id.  A list of `sublime.Region`s will be returned.  If the phantom with the given id is not found, the region will be returned with positions of `(-1, -1)`.  Included for convenience and consistency.

    | Parameter | Type | Required | Default | Description |
    | --------- | ---- | -------- | ------- | ----------- |
    | view | sublime.View | Yes | | A Sublime Text view object. |
    | pid | int | Yes | | The id associated with a single phantom.  Multiple phantoms can share the same key, but each phantom will have its own id. |

    !!! hint "New 1.6.0"
        Feature added in `1.6.0`.

### query_phantoms
[sublime.Region] mdpopups.query_phantoms
: 
    Query the location of multiple phantoms by specifying their ids.  A list of `sublime.Region`s will be returned where each index corresponds to the index of ids that was passed in.  If a given phantom id is not found, that region will be returned with positions of `(-1, -1)`.  Included for convenience and consistency.

    | Parameter | Type | Required | Default | Description |
    | --------- | ---- | -------- | ------- | ----------- |
    | view | sublime.View | Yes | | A Sublime Text view object. |
    | pids | [int] | Yes | | The id associated with a single phantom.  Multiple phantoms can share the same key, but each phantom will have its own id. |

    !!! hint "New 1.6.0"
        Feature added in `1.6.0`.

### class Phantom
mdpopups.Phantoms
: 
    A phantom object for use with [PhantomSet](#class-phantomset).

    | Parameter | Type | Required | Default | Description |
    | --------- | ---- | -------- | ------- | ----------- |
    | region | sublime.Region | Yes | | Region in the view where the phantom should be inserted. |
    | content | string | Yes | | Markdown/HTML content to be used to create a phantom. |
    | layout | int | Yes | | How the HTML content should be inserted.  Acceptable values are: `sublime.LAYOUT_INLINE`, `sublime.LAYOUT_BLOCK`, and `sublime.LAYOUT_BELOW`. |
    | md | bool | No | True | Defines whether the content is Markdown and needs to be converterted. |
    | css | string | No | None | Additional CSS that will be injected. |
    | on_navigate | function | No | None | Callback that receives one variable `href`. |
    | wrapper_class | string | No | None | A string containing the class name you wish wrap your content in.  A `div` will be created with the given class. |
    | template_vars | dict | No | None | A dictionary containing template vars.  These can be used in either the CSS or the HTML/Markdown content. |
    | template_env_options | dict | No | None | A dictionary containing options for the Jinja2 template environment. This **only** applies to the **HTML/Markdown** content. Content plugin vars are found under the object: `plugin`. |
    | nl2br | bool | No | True | Determines whether the newline to br Python Markdown extension is enabled or not. |

    **Attributes**

    | Attribute | Type | Description |
    | --------- | ---- | -------- |
    | region | sublime.Region |  Region in the view where the phantom should be inserted. |
    | content | string |  Markdown/HTML content to be used to create a phantom. |
    | layout | int |  How the HTML content should be inserted.  Acceptable values are: `sublime.LAYOUT_INLINE`, `sublime.LAYOUT_BLOCK`, and `sublime.LAYOUT_BELOW`. |
    | md | bool | Defines whether the content is Markdown and needs to be converterted. |
    | css | string | Additional CSS that will be injected. |
    | on_navigate | function | Callback that receives one variable `href`. |
    | wrapper_class | string | A string containing the class name you wish wrap your content in.  A `div` will be created with the given class. |
    | template_vars | dict | A dictionary containing template vars.  These can be used in either the CSS or the HTML/Markdown content. |
    | template_env_options | dict | A dictionary containing options for the Jinja2 template environment. This **only** applies to the **HTML/Markdown** content. Content plugin vars are found under the object: `plugin`. |
    | nl2br | bool | Determines whether the newline to br Python Markdown extension is enabled or not. |

    !!! hint "New 1.9.0"
        `wrapper_class`, `template_vars`, `template_env_options`, and `nl2br` option added in `1.9.0`.

    !!! hint "New 1.6.1"
        Feature added in `1.6.1`.

### class PhantomSet
mdpopups.PhantomSet
: 
    A class that allows you to update phantoms under the specified key.

    | Parameter | Type | Required | Default | Description |
    | --------- | ---- | -------- | ------- | ----------- |
    | view | sublime.View | Yes | | A Sublime Text view object. |
    | key | string | Yes | | The key that should be associated with all related phantoms in the set. |

    **Methods**

    mdpopups.PhantomSet.update
    : 
        Update all the phantoms in the set with the given phantom list.

        | Parameter | Type | Required | Default | Description |
        | --------- | ---- | -------- | ------- | ----------- |
        | new_phantoms | [[mdpopups.Phantom](#class-phantom)] | Yes | | A list of mdpopup phantoms. `sublime.Phantom` will be converted to `mdpopups.Phantom`. |

    !!! hint "New 1.6.1"
        Feature added in `1.6.1`.

### clear_cache
mdpopups.clear_cache
: 
    Clears the CSS theme related caches.

### md2html
mdpopups.md2html
: 
    Exposes the Markdown to HTML converter in case it is desired to parse only a section of markdown.  This works well for someone who wants to work directly in HTML, but might want to still have fragments of markdown that they would like to occasionally convert. Code highlighting will use either Pygments or the native Sublime syntax highlighter.  Set [`mdpopups.use_sublime_highlighter`](#mdpopupsuse_sublime_highlighter) to `true` if you want to use the Sublime syntax highlighter.

    | Parameter | Type | Required | Default | Description |
    | --------- | ---- | -------- | ------- | ----------- |
    | view | sublime.View |Yes | | Sublime text View object. |
    | markup | string | Yes | | The markup code to be converted. |
    | template_vars | dict | No | None | A dictionary containing template vars.  These can be used in either the CSS or the HTML/Markdown content. |
    | template_env_options | dict | No | None | A dictionary containing options for the Jinja2 template environment. This **only** applies to the **HTML/Markdown** content. Content plugin vars are found under the object: `plugin`. |
    | nl2br | bool | No | True | Determines whether the newline to br Python Markdown extension is enabled or not. |

    !!! hint "New 1.9.0"
        `template_vars`, `template_env_options`, and `nl2br` option added in `1.9.0`.

### color_box
string mdpopups.color_box
: 
    Generates a color preview box image encoded in base64 and formated to be inserted right in your your Markdown or HTML code as an `img` tag.

    | Parameter | Type | Required | Default | Description |
    | --------- | ---- | -------- | ------- | ----------- |
    | colors | [string] | Yes | | A list of color strings formatted as `#RRGGBBAA` where `R` is the red channel, `G` is the green channel, `B` is the blue channel, and `A` is the alpha channel. |
    | border | string | Yes | | The color for the color box border.  Input is a RGB color formatted as `#RRGGBB`. |
    | border2 | string | No | None | The optional secondary border color.  This is great if you are going to have it on a light and dark backgrounds.  You can use a double border so the color stands out regardless of the background.  Input is a RGB color formatted as `#RRGGBB`. |
    | height | int | No | 32 | Height of color box. |
    | width | int | No | 32 | Width of color box. |
    | border_size | int | No | 1 | Width of the color box border.  If using `border2`, the value should be set to at least 2 to see both colors. |
    | check_size | int | No | 4 | Size of checkered box squares used for the background of transparent colors. |
    | max_colors | int | No | 5 | Max number of colors that will be evaluated in the `colors` parameter.  Multiple colors are used to to create palette boxes showing multiple colors lined up horizontally. |
    | alpha | bool | No | False | Will create color box images with a real alpha channel instead of simulating one with a checkered background. |
    | border_map | int | No | 0xF | A mapping of which borders to show.  Where `0x1` is `TOP`, `0x2` is `LEFT`, `0x4` is `BOTTOM`, `0x8` is `RIGHT`.  Map flags can be accessed via `mdpopups.colorbox.TOP` etc. |

### color_box_raw
bytes mdpopups.color_box
: 
    Generates a color preview box image and returns the raw byte string of the image.

    | Parameter | Type | Required | Default | Description |
    | --------- | ---- | -------- | ------- | ----------- |
    | colors | [string] | Yes | | A list of color strings formatted as `#RRGGBBAA` where `R` is the red channel, `G` is the green channel, `B` is the blue channel, and `A` is the alpha channel. |
    | border | string | Yes | | The color for the color box border.  Input is a RGB color formatted as `#RRGGBB`. |
    | border2 | string | No | None | The optional secondary border color.  This is great if you are going to have it on a light and dark backgrounds.  You can use a double border so the color stands out regardless of the background.  Input is a RGB color formatted as `#RRGGBB`. |
    | height | int | No | 32 | Height of color box. |
    | width | int | No | 32 | Width of color box. |
    | border_size | int | No | 1 | Width of the color box border.  If using `border2`, the value should be set to at least 2 to see both colors. |
    | check_size | int | No | 4 | Size of checkered box squares used for the background of transparent colors. |
    | max_colors | int | No | 5 | Max number of colors that will be evaluated in the `colors` parameter.  Multiple colors are used to to create palette boxes showing multiple colors lined up horizontally. |
    | alpha | bool | No | False | Will create color box images with a real alpha channel instead of simulating one with a checkered background. |
    | border_map | int | No | 0xF | A mapping of which borders to show.  Where `0x1` is `TOP`, `0x2` is `LEFT`, `0x4` is `BOTTOM`, `0x8` is `RIGHT`.  Map flags can be accessed via `mdpopups.colorbox.TOP` etc. |

    !!! hint "New 1.7.0"
        Feature dded in `1.7.0`.

### tint
string mdpopups.tint
: 
    Takes a either a path to an png or a byte string of a png and tints it with a specific color and returns a string containing the base64 encoded png in an HTML element.

    | Parameter | Type | Required | Default | Description |
    | --------- | ---- | -------- | ------- | ----------- |
    | img | string/bytes | Yes | | Either a string in the form `Packages/Package/resource.png` or a byte string of a png image. |
    | color | string | Yes | | A string in the form of `#RRGGBB` or `#RRGGBBAA` (alpha layer will be stripped and ignored and is only allowed to make it easy to pass in colors from a color scheme). |
    | opacity | int | No | 255 | An integer value between 0 - 255 that specifies the opacity of the tint. |
    | height | int | No | None | Height that should be specified in the return HTML element. |
    | width | int | No | None | Width that should be specified in the return HTML element. |

    !!! hint "New 1.7.0"
        Feature added in `1.7.0`.

### tint_raw
bytes mdpopups.tint_raw
: 
    Takes a either a path to an png or a byte string of a png and tints it with a specific color and returns a byte string of the modified png.

    | Parameter | Type | Required | Default | Description |
    | --------- | ---- | -------- | ------- | ----------- |
    | img | string/bytes | Yes | | Either a string in the form `Packages/Package/resource.png` or a byte string of a png image. |
    | color | string | Yes | | A string in the form of `#RRGGBB` or `#RRGGBBAA` (alpha layer will be stripped and ignored and is only allowed to make it easy to pass in colors from a color scheme). |
    | opacity | int | No | 255 | An integer value between 0 - 255 that specifies the opacity of the tint. |

    !!! hint "New 1.7.0"
        Feature added in `1.7.0`.

### scope2style
dict mdpopups.scope2style
: 
    Takes a sublime scope (complexity doesn't matter), and guesses the style that would be applied.  While there may be untested corner cases with complex scopes where it fails, in general, it is usually accurate.  The returned dictionary is in the form:

    ```python
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

    | Parameter | Type | Required | Default | Description |
    | --------- | ---- | -------- | ------- | ----------- |
    | view | sublime.View |Yes | | Sublime text View object so that the correct color scheme will be searched. |
    | scope | string | Yes | | The scope to search for. |
    | selected | bool | No | False | Whether this scope is in a selected state (selected text). |
    | explicit_background | bool | No | False | Only return a background if one is explicitly defined in the color scheme. |

    !!! hint "New 1.7.0"
        Feature added in `1.7.0`.

### syntax_highlight
mdpopups.syntax_highlight
: 
    Allows for syntax highlighting outside the Markdown environment.  You can just feed it code directly and give it the language of your choice, and you will be returned a block of HTML that has been syntax highlighted.  This does not have to be in markdown format.  Just give it plain text to convert to highlighted HTML. `syntax_highlight` will use either Pygments or the native Sublime syntax highlighter.  Set [`mdpopups.use_sublime_highlighter`](#mdpopupsuse_sublime_highlighter) to `true` if you want to use the Sublime syntax highlighter.

    | Parameter | Type |Required | Default | Description |
    | --------- | ---- | ------- | ------- | ----------- |
    | view | sublime.View | Yes | | Sublime text View object. |
    | src | string | Yes | | The source code to be converted.  No ` ``` ` needed. |
    | language | string | No | None | Specifies the language to highlight as. |
    | inline | bool |No | False | Will return the code formatted for inline display. |

### get_language_from_view
mdpopups.get_language_from_view
: 
    Allows a user to extract the equivalent language specifier for `mdpopups.syntax_highlight` from a view.  If the language cannot be determined, `None` will be returned.

    | Parameter | Type |Required | Default | Description |
    | --------- | ---- | ------- | ------- | ----------- |
    | view | sublime.View | Yes | | Sublime text View object. |

## Global User Settings
All settings for `MdPopups` are placed in Sublime's `Preferences.sublime-settings`.  They are global and work no for whatever plugin uses the MdPopups API.

### mdpopups.debug
Turns on debug mode.  This will dump out all sorts of info to the console.  Content before parsing to HTML, final HTML output, traceback from failures, etc..  This is more useful for plugin developers.  It works by specifying an error level.  `0` or `false` would disable it.  1 would trigger on errors. 2 would trigger on warnings and any level below.  3 would be general info (like HTML output) and any level below.

```js
    "mdpopups.debug": 1,
```

### mdpopups.disable
Global kill switch to prevent popups (created by MdPopups) from appearing.

```js
    "mdpopups.disable": true,
```

### mdpopups.cache_refresh_time
Control how long a CSS theme file will be in the cache before being refreshed.  Value should be a positive integer greater than 0.  Units are in minutes.  Default is 30.

```js
    "mdpopups.cache_refresh_time": 30,
```

### mdpopups.cache_limit
Control how many CSS theme files will be kept in cache at any given time.  Value should be a positive integer greater than or equal to 0.

```js
    "mdpopups.cache_limit": 10
```

### mdpopups.use_sublime_highlighter
Controls whether the Pygments or the native Sublime syntax highlighter is used for code highlighting.  This affects code highlighting in Markdown conversion] via and when [md2html](#md2html) and when code is directly processed using [syntax_highlight](#syntax_highlight). To learn more about the syntax highlighter see [Syntax Highlighting](#syntax-highlighting).

```js
    "mdpopups.use_sublime_highlighter": true
```

### mdpopups.user_css
Overrides the default CSS theme.  Value should be a relative path pointing to the CSS theme file: `Packages/User/my_custom_theme.css`.  Slashes should be forward slashes. By default, it will point to `Packages/User/mdpopups.css`.  User CSS overrides all CSS: base, default, plugin, etc.

```js
    "mdpopups.use_sublime_highlighter": "Packages/User/mdpopups.css"
```

### mdpopups.default_formatting
Controls whether mdpopups default formatting (contained in [`base.css`](https://github.com/facelessuser/sublime-markdown-popups/blob/master/css/base.css)) will be applied or not.

!!! hint "New 1.9.0"
        Added in `1.9.0`.

### mdpopups.default_style
Controls whether mdpopups default styling (contained in [`default.css`](https://github.com/facelessuser/sublime-markdown-popups/blob/master/css/default.css)) will be applied or not.

!!! hint "New 1.13.0"
        Added in `1.13.0`.

### mdpopups.sublime_user_lang_map
This is a special setting allowing the mapping of personal syntax languages which are not yet included or will not be included in the official mapping table.  You can either define your own new entry, or use the name of an existing entry to extend language keywords or syntax languages.  When extending, user keywords and languages will be cycled through first.

```js
    'mdpopups.sublime_user_lang_map': {
        "language": (('keywords',), ('MyPackage/MySyntaxLanguage'))
    }
```

**Example**:
```js
'mdpopups.sublime_user_lang_map': {
    'javascript': (('javascript', 'js'), ('JavaScript/JavaScript', 'JavaScriptNext - ES6 Syntax/JavaScriptNext'))
}
```

For a list of all currently supported syntax mappings, see the official [mapping file](https://github.com/facelessuser/sublime-markdown-popups/blob/master/st3/mdpopups/st_mapping.py).

### mdpopups.font_scale
Sublime currently doesn't account for font scaling.  For example, if you have a 4K monitor on Windows, and you set the OS font scaling to 125% in your system settings, all your font sizes in your popups and phantoms will be approximately 25% too small.  This feature aims to fix this.  By default, mdpopups will try to guess the scaling on Windows, but it can only guess on Windows currently.  If you don't like what it guesses, or you are on Linux or OSX, you can specify a scale here.

All sizes that are run through the `relativesize` filter use the size of the `font_size` variable found in the a file view's settings as the reference.  It seems that this value is closest to a **px** font size.  `mdpopups.font_scale` will only be applied to sizes run through the `relativesize` filter, and I personally recommend using **px** for fonts (or maybe even **pt** as they seemed all right, but I've seen funny things when using **em** sizes even though seem to calculate fine).

Value should be a positive integer or float.  The default value is `0`. On Windows, if you set it to `0`, it will guess your font scale.  Setting this value to `1` effectively disables it and since you are using a scale of one, no adjustments are made.  On Linux and OSX, either `0` or `1` effectively disables the feature.

So if your OS font scaling is set to 125%, you would probably want to set the scale factor to `1.25` to increase your popup/phantom font sizes to 125% from its calculated 100%.

```js
'mdpopups.font_scale': 0,
```

## Syntax Highlighting
MdPopups has two syntax highlighting methods: one is Pygments, the other is Sublimes native syntax highlighters.  When developing a plugin, it is wise to test out both as a syntax mapping may be needed for the Sublime Syntax Highlighter; mappings can be added locally and/or to the main repository via pull requests.

### Pygments

!!! note "Note"
    Sublime Text 3119 allows for parent and child class in the form `#!css .class1 .class2`.  If you are on 3119 or later, Pygments class will be formatted as `#!css .mdpopups .highight .class, .mdpopups .inline-highlight .class`.  Keep this in mind as you are reading.  The main thing you need to know is that you can customize the background and/or main font color by using the following for ST 3119+:

    ```css
    .mdpopups .highlight, .mdpopups .inline-highlight { background-color: #f8f8f8; color: #4d4d4c }
    ```

    On ST < 3119, you cannot use the parent and child classes, so the CSS is limited:

    ```css
    .highlight, .inline-highlight { background-color: #f8f8f8; color: #4d4d4c }
    ```

Pygments has a great variety of highlighters out of the box.  It also comes with a number of built-in color schemes that can be used.  Pygments themes are loaded up using the [CSS template](#css-templates).  You can either specify an existing one, paste your own in.  Due to the limitations of the Sublime HTML and CSS engine, you must format your personal Pygments them to work well.

Traditionally Pygments CSS classes are given not only syntax classes applied to each span, but an overall class as assigned to a div wrapper as well.  For instance, a class for whitespace may look like this (where `#!css .highlight` is the div wrapper's class and `#!css .w` i the span's class):

```css
.highlight .w { color: #cccccc } /* Text.Whitespace */
```

But the sublime CSS engine doesn't support parent and child classes like `#!css .highlight .w`; it supports either single or multiple classes on one element like `#!css .class1.class2`.  Because of this, the `#!css .highlight` class must be stripped out.

```css
.w { color: #cccccc } /* Text.Whitespace */
```

MdPopups also needs both classes `#!css .highlgiht` and `#!css .inline-highlight` to be styled with the foreground and background color:

```css
.highlight, .inline-highlight { background-color: #f8f8f8; color: #4d4d4c }
```

**Full Example**:

```css
.highlight, .inline-highlight { background-color: #f8f8f8; color: #4d4d4c }
.c { color: #8e908c; font-style: italic } /* Comment */
.err { color: #c82829 } /* Error */
.k { color: #8959a8; font-weight: bold } /* Keyword */
.l { color: #f5871f } /* Literal */
.n { color: #4d4d4c } /* Name */
.o { color: #3e999f } /* Operator */
.p { color: #4d4d4c } /* Punctuation */
.cm { color: #8e908c; font-style: italic } /* Comment.Multiline */
.cp { color: #8e908c; font-weight: bold } /* Comment.Preproc */
.c1 { color: #8e908c; font-style: italic } /* Comment.Single */
.cs { color: #8e908c; font-style: italic } /* Comment.Special */
.gd { color: #c82829 } /* Generic.Deleted */
.ge { font-style: italic } /* Generic.Emph */
.gh { color: #4d4d4c; font-weight: bold } /* Generic.Heading */
.gi { color: #718c00 } /* Generic.Inserted */
.gp { color: #8e908c; font-weight: bold } /* Generic.Prompt */
.gs { font-weight: bold } /* Generic.Strong */
.gu { color: #3e999f; font-weight: bold } /* Generic.Subheading */
.kc { color: #8959a8; font-weight: bold } /* Keyword.Constant */
.kd { color: #8959a8; font-weight: bold } /* Keyword.Declaration */
.kn { color: #8959a8; font-weight: bold } /* Keyword.Namespace */
.kp { color: #8959a8; font-weight: bold } /* Keyword.Pseudo */
.kr { color: #8959a8; font-weight: bold } /* Keyword.Reserved */
.kt { color: #eab700; font-weight: bold } /* Keyword.Type */
.ld { color: #718c00 } /* Literal.Date */
.m { color: #f5871f } /* Literal.Number */
.s { color: #718c00 } /* Literal.String */
.na { color: #4271ae } /* Name.Attribute */
.nb { color: #4271ae } /* Name.Builtin */
.nc { color: #c82829; font-weight: bold } /* Name.Class */
.no { color: #c82829 } /* Name.Constant */
.nd { color: #3e999f } /* Name.Decorator */
.ni { color: #4d4d4c } /* Name.Entity */
.ne { color: #c82829; font-weight: bold } /* Name.Exception */
.nf { color: #4271ae; font-weight: bold } /* Name.Function */
.nl { color: #4d4d4c } /* Name.Label */
.nn { color: #4d4d4c } /* Name.Namespace */
.nx { color: #4271ae } /* Name.Other */
.py { color: #4d4d4c } /* Name.Property */
.nt { color: #c82829 } /* Name.Tag */
.nv { color: #c82829 } /* Name.Variable */
.ow { color: #3e999f } /* Operator.Word */
.w { color: #4d4d4c } /* Text.Whitespace */
.mb { color: #f5871f } /* Literal.Number.Bin */
.mf { color: #f5871f } /* Literal.Number.Float */
.mh { color: #f5871f } /* Literal.Number.Hex */
.mi { color: #f5871f } /* Literal.Number.Integer */
.mo { color: #f5871f } /* Literal.Number.Oct */
.sb { color: #718c00 } /* Literal.String.Backtick */
.sc { color: #4d4d4c } /* Literal.String.Char */
.sd { color: #8e908c } /* Literal.String.Doc */
.s2 { color: #718c00 } /* Literal.String.Double */
.se { color: #f5871f } /* Literal.String.Escape */
.sh { color: #718c00 } /* Literal.String.Heredoc */
.si { color: #f5871f } /* Literal.String.Interpol */
.sx { color: #718c00 } /* Literal.String.Other */
.sr { color: #718c00 } /* Literal.String.Regex */
.s1 { color: #718c00 } /* Literal.String.Single */
.ss { color: #718c00 } /* Literal.String.Symbol */
.bp { color: #f5871f } /* Name.Builtin.Pseudo */
.vc { color: #c82829 } /* Name.Variable.Class */
.vg { color: #c82829 } /* Name.Variable.Global */
.vi { color: #c82829 } /* Name.Variable.Instance */
.il { color: #f5871f } /* Literal.Number.Integer.Long */
```

### Sublime Syntax Highlighter
MdPopups can also use Sublime's internal syntax highlighter to highlight your code.  The benefit here is that you get code highlighting in your popup that matches your current theme.  The highlighting ability is dependent upon what syntax packages you have installed in Sublime.  It also depends on whether they are enabled and mapped to a language keyword.  Pull requests are welcome to expand and keep the [language mapping](https://github.com/facelessuser/sublime-markdown-popups/blob/master/st3/mdpopups/st_mapping.py) updated.  You can also define in your `Preferences.sublime-settings` file additional mappings to either personal syntax files, or while waiting for your mapping changes to be merged and released.  See [`mdpopups.sublime_user_lang_map`](#mdpopupssublime_user_lang_map) for more info.

In your CSS template it is usually a good idea to generically specify the code wrapper background colors.  With the [CSS templates](#css-templates), this is very easy:

```css+jinja
.highlight, .inline-highlight { {{'.background'|css}} }
```

## CSS Styling
MdPopups was design to give a universal way of displaying and styling tooltips and phantoms via plugins, but also provide the user an easy way to control the look.

MdPopups provides a simple base CSS that styles the basic HTML tags that can be used in the Markdown parser.  On top of that it then parses your current Sublime color scheme and generates CSS that includes styling for all the [standard TextMate scopes](./textmate_scopes.md) (and only those listed scopes) found in your color scheme.  It then uses those scopes in a default template to highlight your tooltips and phantoms to match your current color scheme.

Templates are used so that a user can easily tap into all the colors, color filters, and other useful logic to control their tooltips and phantoms in one place without having to hard code a specific CSS for a specific color scheme.  Even though a plugin can additionally insert new scopes on demand when calling the popup API, a user can override anything and everything by providing their own [CSS template](#mdpopupsuser_css).  The template is fairly powerful and flexible.

## CSS Templates
MdPoups provides a [`base.css`](https://github.com/facelessuser/sublime-markdown-popups/blob/master/css/base.css) that formats the general look of the HTML elements (padding, size, etc.).  On top of that, it provides a [`default.css`](https://github.com/facelessuser/sublime-markdown-popups/blob/master/css/default.css) template which applies more superficial styling such as colors, Pygments themes, etc.  It uses the Jinja2 template environment to give direct access to things like color scheme colors, names, and other useful information.  In general, `base.css` and `default.css` should provide most of what everyone **needs**.  But if you **want** greater control, you can create your own CSS template which MdPopups which will override anything in `base.css` and `default.css`.

All variables and filters below only apply the CSS, not the content.  The content only receives the variables **you** give it via `template_vars` and any options and filters you give it via the `template_env_options`.  The css will receive the variables fed in through `template_vars`, and in the case of CSS and content, both will place the plugin variables under the object `plugin`.

!!! hint "New 1.13.0"
    User CSS now overrides `base.css` and `default.css` instead of replacing `default.css`.  You can get the same effect as legacy by disabling [default styling](#) and creating your own user CSS.

### Sizes Relative to View's Font Size

!!! caution "Notice"
    It is recommended moving forward (starting with mdpopups version 1.8.0 and SublimeText build 3119) to use `rem` units for relative sizes.  If you need to dynamically choose whether to use `rem` or  not, you can check the template variable `var.sublime_version`.

Sizes can be defined relative to the current Sublime file view's font size.  An example would be ensuring font sizes in a popup or phantom match the size of the font in the Sublime Text file view.  The sizes that can be adjusted are `pt`, `em`, `px`.

relativesize
: 
    Takes a relative specifier and inserts the size in the provided unit relative to the font size in the current Sublime Text file view. The filter is applied to a string that consists of a leading relative operator (`+`, `-`, or `*`), a positive number, and one of three size types (`em`, `px`, or `pt`). `relativesize` can also take a boolean to turn the the float value into a rounded int.

    | Operator | Description |
    |----------|-------------|
    | `+` | Adds the specified value to the current font size. |
    | `-` | Subtracts the specified value from the current font size. |
    | `*` | Multiplies the value to the current font size. This allows both dividing and multiplying the font size by a given factor.  To cut in half: `*.5`.  To double the size `*2`. |

    **Example**:

    ```css+jinja
    h1 { font-size: {{'+5px'|relativesize}}; }
    h2 { font-size: {{'+4px'|relativesize}}; }
    h3 { font-size: {{'+3px'|relativesize}}; }
    h4 { font-size: {{'+2px'|relativesize}}; }
    h5 { font-size: {{'+1px'|relativesize}}; }
    h6 { font-size: {{'+0px'|relativesize}}; }
    ```

    Would become this (assuming a font size of 19px):

    ```css+jinja
    h1 { font-size: 24px; }
    h2 { font-size: 23px; }
    h3 { font-size: 22px; }
    h4 { font-size: 21px; }
    h5 { font-size: 20px; }
    h6 { font-size: 19px; }
    ```

    **Example - Integer Rounding**

    ```css+jinja
    ul, ol { padding-left: {{'*.5em'|relativesize(True)}}; }
    ```

    Would become this (assuming a font size of 19px):

    ```css+jinja
    ul, ol { padding-left: 1em; }
    ```

    The conversion factor between **px**, **pt**, and **em** is assummed to be 16px --> 1em --> 12pt.  Whether this is what sublime is actually doing is another question.  We assume that the Sublime `font_size` setting is in **px** as this has given the best overall feel.  **em** are not recommened for font sizes as I've seen some strange behaviour when scaling **em** (even though the numbers seem to calculate correctly).  **em** issues may not exists with elements that are not font, but please report any issues you find.

    !!! hint "New 1.7.2"
        Integer rounded added in `1.7.2`.  Rounding not supported in old style call from `1.7.0`.

    !!! hint "New 1.7.1"
        `1.7.1` introduced the more simple format of `{{'+5px'|relativesize}}`.  It is encouraged to adopt this format instead of `1.7.0` format as it will be removed in the future.

    !!! hint "New 1.7.0"
        Added in `1.7.0`.

        This was the `1.7.0` format which was cumbersome: `{{'+5'|relativesize('px')}}`.  In `1.7.1`, it changed, but the old way is still supported.

### Template Colors
With the template environment, colors from the current Sublime color scheme can be accessed and manipulated.  Access to the Sublime color scheme styles are done via the `css` filter.

css
: 
    Retrieves the style for a specific TextMate scope from a Sublime color scheme.  By specifying either `.foreground`, `.background`, or anyone of the standard TextMate scopes and then paring it with the `css` filter, all the related styles of the specified scope will be inserted into the css document.

    **Example**:

    This:

    ```css+jinja
    h1, h2, h3, h4, h5, h6 { {{'.comment'|css}} }
    ```

    Might become this:

    ```css+jinja
    h1, h2, h3, h4, h5, h6 { color: #888888; font-style: italic; }
    ```

    Notice that the format of insertion is `key: value; `.  You do not need a semicolon after.  If you add one, you may get multiple semicolons which may break some things.

    If you need to get at a specific CSS attribute, you can specify its name in the `css` filter (available attributes are `color`, `background-color`, `font-style`, and `font-weight`).

    This:

    ```css+jinja
    h1, h2, h3, h4, h5, h6 { {{'.comment'|css('color')}} }
    ```

    Would then only include the color:

    ```css+jinja
    h1, h2, h3, h4, h5, h6 { color: #888888 }
    ```

    Some scopes might not have colors assigned to them, so multiple scopes can be defined, and the first one that matches will be used:

    ```css+jinja
    /* If `keyword.operator` is not explicitly used, fallback to `.keyword` */
    h1, h2, h3, h4, h5, h6 { {{'.keyword.operator, .keyword'|css('color')}} }
    ```

If desired you can convert a foreground color to a background color or vice versa.  To convert to a foreground color, you can use the `foreground` filter.  To convert to a background color, you can use the `background` filter.

foreground
: 
    Convert a background to a foreground.

    **Example**:
    ```css+jinja
    body { {{'.background'|css('background-color')|foreground}} }
    ```

background
: 
    Convert a foreground to a background.

    **Example**:
    ```css+jinja
    body { {{'.foreground'|css('color')|background}} }
    ```

### Template Color Filtering
MdPopups also provides a number of color filters within the template environment that can manipulate the colors.  For instance, lets say you had your tooltip in the same color as the view window and it was difficult to see where the tooltip starts and ends.  You can take the color schemes background and apply a brightness filter to it allowing you now see the tooltip clearly.

Here we can make the background of the tooltip darker:

```css+jinja
body { {{'.background'|css('background-color')|brightness(0.9)}} }
```

Color filters take a single color attribute of the form `key: value;`.  So when feeding the filter with CSS, it is advised to specify the attribute in the `css` filter to limit the return to only one attribute as shown above; it may be difficult to tell how many attributes `css` could return without explicitly specifying attribute.  Color filters only take either `color` or `background-color` attributes.

Filters can be chained if more intensity is needed as some filters may clamp the value in one call. Multiple kinds of filters can also be chained together.  These are all the available filters:

brightness
: 
    Shifts brightness either dark or lighter. Brightness is relative to 1 where 1 means no change.  Accepted values are floats that are greater than 0.  Ranges are clamped between 0 and 2.

    **Example - Darken**:
    ```css+jinja
    body { {{'.background'|css('background-color')|brightness(0.9)}} }
    ```

    **Example - Lighten**:
    ```css+jinja
    body { {{'.background'|css('background-color')|brightness(1.1)}} }
    ```

saturation
: 
    Shifts the saturation either to right (saturate) or the left (desaturate).  Saturation is relative to 1 where 1 means no change.  Accepted values are floats that are greater than 0.  Ranges are clamped between 0 and 2.

    **Example - Desaturate**:
    ```css+jinja
    body { {{'.background'|css('background-color')|saturation(0.9)}} }
    ```

    **Example - Saturate**:
    ```css+jinja
    body { {{'.background'|css('background-color')|saturation(1.1)}} }
    ```

grayscale
: 
    Filters all colors to a grayish tone.

    **Example**:
    ```css+jinja
    body { {{'.background'|css('background-color')|grayscale}} }
    ```

sepia
: 
    Filters all colors to a sepia tone.

    **Example**:
    ```css+jinja
    body { {{'.background'|css('background-color')|sepia}} }
    ```

invert
: 
    Inverts a color.

    **Example**:
    ```css+jinja
    body { {{'.background'|css('background-color')|invert}} }
    ```

colorize
: 
    Filters all colors to a shade of the specified hue.  Think grayscale, but instead of gray, you define a non-gray hue.  The values are angular dimensions starting at the red primary at 0°, passing through the green primary at 120° and the blue primary at 240°, and then wrapping back to red at 360°.

    **Example**:
    ```css+jinja
    body { {{'.background'|css('background-color')|colorize(30)}} }
    ```

hue
: 
    Shifts the current hue either to the left or right.  The values are angular dimensions starting at the red primary at 0°, passing through the green primary at 120° and the blue primary at 240°, and then wrapping back to red at 360°.  Values can either be negative to shift left or positive to shift the hue to the right.

    **Example - Left Shift**:
    ```css+jinja
    body { {{'.background'|css('background-color')|hue(-30)}} }
    ```

    **Example - Left Right**:
    ```css+jinja
    body { {{'.background'|css('background-color')|hue(30)}} }
    ```

fade
: 
    Fades a color. Essentially it is like apply transparency to the color allowing the color schemes base background color to show through.

    **Example - Fade 50%**:
    ```css+jinja
    body { {{'.foreground'|css('color')|fade(0.5)}} }
    ```

### Include CSS
The template environment allows for retrieving built-in Pygments CSS or retrieving CSS resources from the Sublime Packages.

pygments
: 
    Retrieve a built-in Pygments color scheme.

    **Example**:
    ```css+jinja
    {{'native'|pygments}}
    ```

getcss
: 
    Retrieve a CSS file from Sublime's `Packages` folder.  CSS retrieved in this manner can include template variables and filters.

    **Example**:
    ```css+jinja
    {{'Packages/User/aprosopo-dark.css'|getcss}}
    ```

## Template Variables
The template environment provides a couple of variables that can be used to conditionally alter the CSS output.  Variables are found under `var`.

var.sublime_version
: 
    `sublime_version` contains the current SublimeText version.  This allows you conditionally handle CSS features that are specific to a SublimeText version.

    **Example**
    ```css+jinja
    {% if var.sublime_version >= 3119 %}
    padding: 0.2rem;
    {% else %}
    padding: 0.2em;
    {% endif %}
    ```

    !!! hint "New 1.8.0"
        Added in `1.8.0`.

var.mdpopups_version
: 
    `mdpopups_version` contains the current mdpopup version which you can use in your CSS templates if needed.

    **Example**
    ```css+jinja
    {% if var.mdpopups_version >= (1.9.0) %}
    /* do something */
    {% else %}
    /* do something else */
    {% endif %}
    ```

    !!! hint "New 1.9.0"
        Added in `1.9.0`.

var.default_formatting
: 
    Flag specifying whether default formatting is being used.  See [mdpopups.default_formatting](#mdpopupsdefault_formatting) for how to control this flag.  And see [`base.css`](https://github.com/facelessuser/sublime-markdown-popups/blob/master/css/base.css) for an example of how it is used.

    !!! hint "New 1.9.0"
        Added in `1.9.0`.

var.default_style
: 
    Flag specifying whether default styling is being used.  See [mdpopups.default_style](#mdpopupsdefault_style) for how to control this flag.  And see [`default.css`](https://github.com/facelessuser/sublime-markdown-popups/blob/master/css/default.css) for an example of how it is used.

    !!! hint "New 1.13.0"
        Added in `1.13.0`.

var.is_dark | var.is_light
: 
    `is_dark` checks if the color scheme is a dark color scheme.  Alternatively, `is_light` checks if the color scheme is a light color scheme.

    **Example**:
    ```css+jinja
    {% if var.is_light %}
    html{ {{'.background'|css('background-color')|brightness(0.9)}} }
    {% else %}
    html{ {{'.background'|css('background-color')|brightness(1.1)}} }
    {% endif %}
    ```

var.is_popup | var.is_phantom
: 
    `is_phantom` checks if the current CSS is for a phantom instead of a popup.  Alternatively, `is_popup` checks if the current use of the CSS is for a popup.

    **Example**:
    ```css+jinja
    {% if var.is_phantom %}
    html{ {{'.background'|css('background-color')|brightness(0.9)}} }
    {% else %}
    html{ {{'.background'|css('background-color')|brightness(1.1)}} }
    {% endif %}
    ```

    !!! hint "New 1.6.0"
        Added in `1.6.0`.

var.use_pygments
: 
    Checks if the Pygments syntax highlighter is being used.

    **Example**:
    ```css+jinja
    {% if var.use_pygments %}
    {% if var.is_light %}
    {{'default'|pygments}}
    {% else %}
    {{'native'|pygments}}
    {% endif %}
    {% endif %}
    ```

var.color_scheme
: 
    Retrieves the current color schemes name.

    **Example**:
    ```css+jinja
    {% if (
        var.color_scheme in (
            'Packages/Theme - Aprosopo/Tomorrow-Night-Eighties-Stormy.tmTheme',
            'Packages/Theme - Aprosopo/Tomorrow-Morning.tmTheme',
        )
    ) %}
    a { {{'.keyword.operator'|css('color')}} }
    {% else %}
    a { {{'.support.function'|css('color')}} }
    {% endif %}
    ```
