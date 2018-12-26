# ESLint for Sublime Text

Lint ECMAScript/JavaScript syntax by [ESLint][ESLint Official] in [Sublime Text 2][Sublime Text 2] and [3][Sublime Text 3].

## Prerequisites

* [Sublime Package Control][Package Control]
* [Node.js][Node.js]
* [eslint][ESLint Official GitHub]

## Installation

### Install Node.js and eslint

Before using this plugin, you must ensure that `eslint` is installed on your system.
To install `eslint`, do the following:

1. Install [Node.js][Node.js] (and [npm][npm] on Linux).

2. Install `eslint` globally by typing the following in a terminal:
   ```bash
   npm install -g eslint
   ```

### Install plugin

Install this plugin by using Sublime Text [Package Control][Package Control].

1. Open **"Command Pallet"** <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>p</kbd> (<kbd>Cmd</kbd> + <kbd>Shift</kbd> + <kbd>p</kbd> on OSX)
2. Select **"Package Control: Install Package"**
3. Select **ESLint**

## Run ESLint

ESLint an active JavaScript file.


* Open the context menu (right-click), and Select **ESLint**,  
  Or Open "Command Pallet" and Select **ESLint**,  
  Or keyboard shortcut: <kbd>Ctrl</kbd> + <kbd>Alt</kbd> + <kbd>e</kbd> (<kbd>Cmd</kbd> + <kbd>Option</kbd> + <kbd>e</kbd> on OSX)

* <kbd>F4</kbd> : Jump to next error row/column
* <kbd>Shift</kbd> + <kbd>F4</kbd> : Jump to previous error row-column

**Note:**
The <kbd>Ctrl</kbd> + <kbd>Alt</kbd> + <kbd>e</kbd> (<kbd>Cmd</kbd> + <kbd>Option</kbd> + <kbd>e</kbd> on OSX) shortcut changes the Build System on the current file to ESLint,
then Builds to run ESLint on the file and output any errors for jumping to within the file.
You could alternatively set the Build System to Automatic and <kbd>Ctrl</kbd> + <kbd>b</kbd> (<kbd>Cmd</kbd> + <kbd>b</kbd> on OSX) or <kbd>F7</kbd>,
but only on files that end with `.js`.

## Configuring ESLint

[ESLint][ESLint Official] allows you to specify the JavaScript language options you want to support by using `.eslintrc` file,
it will use the first `.eslintrc` file found traversing from the active file in Sublime Text up to your project's root.

You can configure ESLint options by specify `.eslintrc` file.
For more information, see the [ESLint docs][ESLint Official Configuration Docs].

## Settings

Several settings are available to customize the plugin's behavior.
Those settings are stored in a configuration file, as JSON.

Go to "`Preferences` / `Package Settings` / `ESLint` / `Settings - User`" to add your custom settings.

### node_path

*Default: `""`*

The directory location of your `node` executable lives.
If this is not specified, then it is expected to be on Sublime's environment path.

### node_modules_path

*Default: `""`*

The directory location of global `node_modules` via `npm`.
If this is not specified, then it is expected to be on system environment variable `NODE_PATH`.

### config_file

*Default: `""`*

This option allows you to specify an additional configuration file for ESLint.
If not specified, follows the default config file hierarchy.
This option works same as ESLint `-c` or `--config` command line option.

For more information, see the [ESLint docs][ESLint Official Specifying Basic Configuration File Docs].


Example:

```javascript
{
  "node_path": "/usr/local/bin",
  "node_modules_path": "/usr/local/lib/node_modules",
  "config_file": "/path/to/.eslintrc.js"
}
```

## ESLint on save

Install [SublimeOnSaveBuild][SublimeOnSaveBuild]


[ESLint Official]: http://eslint.org/
[ESLint Official Configuration Docs]: http://eslint.org/docs/user-guide/configuring#configuration-file-formats
[ESLint Official Specifying Basic Configuration File Docs]: http://eslint.org/docs/user-guide/command-line-interface#basic-configuration
[Sublime Text 2]: http://www.sublimetext.com/2
[Sublime Text 3]: http://www.sublimetext.com/3
[ECMAScript 6]: http://www.ecma-international.org/publications/standards/Ecma-262.htm
[React]: https://facebook.github.io/react/
[JSX]: https://facebook.github.io/jsx/
[Package Control]: http://wbond.net/sublime_packages/package_control/installation
[Node.js]: https://nodejs.org/
[ESLint Official GitHub]: https://github.com/eslint/eslint
[npm]: https://nodejs.org/en/download/package-manager/
[SublimeOnSaveBuild]: https://github.com/alexnj/SublimeOnSaveBuild
[React plugin]: https://github.com/yannickcr/eslint-plugin-react
