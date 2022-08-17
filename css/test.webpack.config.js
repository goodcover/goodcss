module.exports = (env) => {
  const name = "css"
  const baseDir = __dirname
  const common = require('../client-comp/common-test.webpack.config')
  return common(baseDir, name, env);
};
