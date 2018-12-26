"""
Mdpopups manual test module.

On load, it will clear the cache and show the test menu.
Subsequent tests can be loaded:

    mdpopups.tests.menu()

If you need to reload the test module.

    import imp
    mdpopups.tests = imp.reload(mdpopups.tests)

If you need to clear the cache.

    mdpopups.tests.clear_cache()
"""
import sublime
import mdpopups
from textwrap import dedent
import sys
this = sys.modules[__name__]

format_text = dedent(
    '''
    # H1
    ## H2
    ### H3
    #### H4
    ##### H5
    ###### H6

    ---

    ## Paragraphs
    Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut \
    labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco \
    laboris nisi ut aliquip ex ea commodo consequat...

    Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut \
    labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco \

    ## Inline
    Inline **bold**.

    Inline *italic*.

    Inline `code`.

    Inline `#!python import code`

    [mdpopups link](https://github.com/facelessuser/sublime-markdown-popups)

    ## Line Breaks and Quotes

    > This is a line break.
    > This is a line.

    ## Lists

    Apple
    : 1) The round fruit of a tree of the rose family, which typically has thin red or green skin and crisp flesh.

        Many varieties have been developed as dessert or cooking fruit or for making cider.

    : 2) The tree which bears apples.

    - Fruit
        - Apples
        - Bannanas
        - Oranges
        - Grapes

    1. Meat
        1. Chicken
        2. Pork
        3. Beef

    # Blocks

        Indented code block
        goes here

    ```python
    # Fenced code block
    import awesome
    ```

    - Nested Fence:

        ```python
        # Fenced code block
        import awesome
        ```

    !!! caution "Admontion"
        Admonition block

    '''
)


def active_view():
    """Get active view."""
    return sublime.active_window().active_view()


def clear_cache():
    """Clear CSS cache."""
    mdpopups.clear_cache()


def menu():
    """Show menu allowing you to select a test."""
    tests = (
        "Popup Format",
        "Phantom Format"
    )

    def run_test(value):
        """Run the test."""
        if value >= 0:
            test = '_'.join(tests[value].lower().split(' '))
            getattr(this, 'mdpopups_%s_test' % test)()

    window = active_view().window()
    window.show_quick_panel(tests, run_test)


def on_close_popup(href):
    """Close the popup."""
    view = active_view()
    mdpopups.hide_popup(view)


def on_close_phantom(href):
    """Close all phantoms."""
    view = active_view()
    mdpopups.erase_phantoms(view, 'mdpopups_test')


def show_popup(text):
    """Show the popup."""
    close = '\n[(close)](#)'
    view = active_view()
    region = view.visible_region()
    mdpopups.show_popup(active_view(), text + close, location=region.a, on_navigate=on_close_popup)


def show_phantom(text):
    """Show the phantom."""
    close = '\n[(close)](#)'
    view = active_view()
    region = view.visible_region()
    mdpopups.add_phantom(
        active_view(), 'mdpopups_test', region, text + close, 2, on_navigate=on_close_phantom
    )


def mdpopups_popup_format_test():
    """Test popup."""
    show_popup(format_text)


def mdpopups_phantom_format_test():
    """Test phantom."""
    show_phantom(format_text)

clear_cache()
menu()
