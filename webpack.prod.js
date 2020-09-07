const path = require("path");
const { CleanWebpackPlugin } = require("clean-webpack-plugin");
const HtmlWebpackPlugin = require("html-webpack-plugin");

module.exports = {
  mode: "production",
  entry: {
    app: path.resolve(__dirname, "src/index.tsx"),
  },

  module: {
    rules: [
      {
        test: /\.(ts|js)x?$/,
        use: "babel-loader",
        exclude: [/node_modules/],
      },
      {
        test: /\.css$/i,
        exclude: [/node_modules/],
        use: ["style-loader", "css-loader", "postcss-loader"],
      },
    ],
  },
  resolve: {
    extensions: [".ts", ".tsx", ".js", ".jsx", ".json"],
  },
  plugins: [
    new CleanWebpackPlugin(),
    new HtmlWebpackPlugin({
      filename: "./index.html",
      template: "public/index.html",
    }),
  ],
  output: {
    path: path.resolve(__dirname, "dist/"),
    filename: "[name].[contenthash].js",
  },
};
