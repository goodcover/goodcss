module.exports = (env) => {
  const baseDir = __dirname
  const common = require('../webpack/common-test.webpack.config')
  return common(baseDir, env);
};
