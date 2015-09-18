var path = require('path');

exports.root = path.join(path.dirname(process.argv[1]), '..');

exports.elmPackage = path.join(exports.root, 'elm-package.json');
var elmPackageContents = require(exports.elmPackage);

exports.sourceElm = path.join(exports.root, elmPackageContents['source-directories'][0]);
exports.rootModule = path.join(exports.sourceElm, elmPackageContents['exposed-modules'][0]);

exports.target = path.join(exports.root, 'target');
exports.targetMain = path.join(exports.target, 'main');

exports.sourceMainRoot = path.join(exports.rootModule, 'Main.elm')
exports.targetMainRoot = path.join(exports.targetMain, 'index.html')
