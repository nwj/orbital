const path = require('path');
const { CleanWebpackPlugin } = require('clean-webpack-plugin');
const HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = {
  entry: {
    app: './src/index.js'
  },
  plugins: [
    new CleanWebpackPlugin(),
    new HtmlWebpackPlugin({
      title: "Orbital"
    })
  ],
  module: {
    rules: [
      {
        test: /\.css$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: [
            { loader: "style-loader" },
            { loader: "css-loader" }
        ]
      },
    ]
  },
  output: {
    path: path.resolve(__dirname, 'dist')
  }
};
