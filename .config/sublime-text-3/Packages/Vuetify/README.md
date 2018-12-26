## WORK IN PROGRESS

<p align="center">
  <img height="200px"
  src="https://res.cloudinary.com/confidante/image/upload/v1520961320/logo_ew2tpg.png">
</p>

<h2 align="center">vuetify-sublime</h2>
<p align="center">Vuetify Sublime Text extension</p>

`vuetify-sublime` is the official extension for Vuetifyjs when working in Sublime Text. It provides snippets and autocomplete functionality for Vuetifyjs.

## Documentation

For all information about available snippets and autocompletion for Sublime Text [click here](https://github.com/vuetifyjs/vuetify-sublime/blob/master/documentation.md)

## Installation

#### Package Control

1. Open your **Command Palette** _(Command + Shift + P)_
2. Search for **"Vuetify"** to get it from Package Control.

## Usage

#### Autocompletion

Typing `<` will provide not with all available components and when you hit tab, the autompletion will work like a snippet.

#### Snippet

You don't need usage example if you are familiar with concept of snippets or you looked up the documentation. But here is an example:

Let's suppose you want to insert Button componenet. For that you have to write full component.

```HTML
<v-btn>buttonText<v-btn>
```

But in `vuetify-sublime` only writing `vBtn` will give you all options available for Button component.

Everything is in camel case and with 'v' prefix which is for Vuetifyjs.

If you want to insert a component with all of its props. Just write `v{component-name}WithProps`. This will insert component with all of its props. For example : `vBtnWithProps`, `vAvatarWithProps` etc.

#### Templates

Every component in the Vuetify have additional code to write inside it. For example the `v-btn-toggle` component have `v-btn` inside it. Thats why `vuetify-sublime` provides templates for them.
The syntax of template is pretty easy. `v{component}Template` or `v{component}Template{availableTemplate}`
For example `vBtnToggleTemplate` will give you following code.

```HTML
<v-btn-toggle mandatory multiple v-model="value">
  <v-btn flat>
    <v-icon>icon</v-icon>
  </v-btn>
  <v-btn flat>
    <v-icon>icon</v-icon>
  </v-btn>
  <v-btn flat>
    <v-icon>icon</v-icon>
  </v-btn>
  <v-btn flat>
    <v-icon>icon</v-icon>
  </v-btn>
</v-btn-toggle>
```

and `vBtnToggleTemplateTextIcon` will give you following code.

```HTML
<v-btn-toggle multiple mandatory v-model="value">
  <v-btn flat value="value">
    <span>text</span>
    <v-icon>icon</v-icon>
  </v-btn>
  <v-btn flat value="value">
    <span>text</span>
    <v-icon>icon</v-icon>
  </v-btn>
  <v-btn flat value="value">
    <span>text</span>
    <v-icon>icon</v-icon>
  </v-btn>
  <v-btn flat value="value">
    <span>text</span>
    <v-icon>icon</v-icon>
  </v-btn>
</v-btn-toggle>
```

## Changelog

<a href="https://github.com/vuetifyjs/vuetify-sublime/blob/master/CHANGELOG.md" target="_blank">Changelog</a>

## Questions

If you have any questions, ideas or you want to discuss with me. Just raise a issue in `vuetify-sublime` [github repository](https://github.com/vuetifyjs/vuetify-sublime/issues).

## License

[MIT](https://github.com/vuetifyjs/vuetify-sublime/blob/master/LICENSE)
