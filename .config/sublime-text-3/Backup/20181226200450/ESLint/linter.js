'use strict';

var fs = require('fs');
var path = require('path');
var args = process.argv.slice(2);

var targetPath = args[0];
var targetDir = path.dirname(targetPath);

var nodeModulesPath = args[1];
if (nodeModulesPath) {
  module.paths.push(nodeModulesPath);
}
var configFile = args[2];

var eslintPath = path.join(targetDir, 'node_modules', 'eslint');
var eslint;
if (fs.existsSync(eslintPath)) {
  eslint = require(eslintPath);
} else {
  eslint = require('eslint');
}

var CLIEngine = eslint.CLIEngine;
var options = {};
if (configFile) {
  options.configFile = configFile;
}
var cli = new CLIEngine(options);

var report = cli.executeOnFiles([targetPath]);
// eslint-disable-next-line no-console
console.log(format(report.results));


function format(results) {
  var lines = [];

  function numberWang(wangaNumb) {
    var thatsNumberWang = 7 - wangaNumb;
    var stayNumberWang = '';
    var i;

    for (i = 0; i < thatsNumberWang; i++) {
      stayNumberWang += ' ';
    }

    return stayNumberWang;
  }

  lines.push('[ESLint: ' + results[0].filePath + ']');
  lines.push('');

  var messages = results[0].messages;
  var errorCount = results[0].errorCount || 0;
  var warningCount = results[0].warningCount || 0;

  if (errorCount || warningCount) {

    messages.forEach(function(error) {
      var ruleId = error.ruleId ? ' (' + error.ruleId + ')' : '';
      var severity = (error.severity === 1 ? 'Warn ' : 'Error');

      lines.push([
        '\t',
        severity,
        numberWang((error.line + error.column.toString()).length),
        error.line + ',' + error.column + ':',
        error.message + ruleId
      ].join(' '));
    });

    lines.push('');
    lines.push('✗ ' +
      errorCount + ' ' + (errorCount === 1 ? 'error' : 'errors') + ', ' +
      warningCount + ' ' + (warningCount === 1 ? 'warning' : 'warnings'));
    lines.push('');
    lines.push('Double-click on lines to jump to location, [F4] for next, [shift-F4] for previous.'
    );
  } else {
    lines.push('✓ 0 errors and warnings, [esc] to hide.');
  }

  lines.push('');
  return lines.join('\n');
}
