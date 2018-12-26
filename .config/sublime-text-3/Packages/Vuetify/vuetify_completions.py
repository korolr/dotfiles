import sublime_plugin
import sublime
from .completions import color_classes, components


class VuetifyCompletions(sublime_plugin.EventListener):
    """
    Provide tag completions for Vuetify elements and v-{component} attributes
    """

    def __init__(self):
        self.color_class_completions = [
            ("%s \tColor class - Vuetify" % s, s) for s in color_classes]

    def on_query_completions(self, view, prefix, locations):

        if view.match_selector(locations[0], "text.html string.quoted"):
            # Cursor is inside a quoted attribute
            # Now check if we are inside the class attribute

            # max search size
            LIMIT = 250

            # place search cursor one word back
            cursor = locations[0] - len(prefix) - 1

            # dont start with negative value
            start = max(0, cursor - LIMIT - len(prefix))

            # get part of buffer
            line = view.substr(sublime.Region(start, cursor))

            # split attributes
            parts = line.split('=')

            # is the last typed attribute a color attribute?
            if len(parts) > 1 and parts[-2].strip().endswith("color"):
                return self.color_class_completions
            else:
                return []
        elif view.match_selector(locations[0], "text.html - source"):
            pt = locations[0] - len(prefix) - 1
            ch = view.substr(sublime.Region(pt, pt + 1))
            if ch != '<':
                return []
            else:
                return (components, sublime.INHIBIT_WORD_COMPLETIONS | sublime.INHIBIT_EXPLICIT_COMPLETIONS)

        else:

            return []
