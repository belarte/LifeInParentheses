module.exports = function (config) {
    config.set({
        browsers: ['ChromeHeadless'],
        basePath: 'target', // this is the same as the base-path of `:output-to` in `shadow-cljs.edn`
        files: [
            'https://code.jquery.com/jquery-2.1.1.min.js',
            'https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js',
            'test.js' // this is the same as the file-name (ending with .js) of `:output-to` in `shadow-cljs.edn`
        ],
        frameworks: ['cljs-test'],
        plugins: ['karma-cljs-test', 'karma-chrome-launcher'],
        colors: true,
        logLevel: config.LOG_INFO,
        client: {
            args: ["shadow.test.karma.init"],
            singleRun: true
        }
    })
};
