module.exports = (env) => {
  const baseDir = __dirname
  const common = require('../client-comp/common-test.webpack.config')
  return common(baseDir, env);
};
