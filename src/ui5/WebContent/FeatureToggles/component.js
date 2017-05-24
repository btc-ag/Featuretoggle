$.sap.require("sap.ui.core.UIComponent");
$.sap.declare("FeatureToggles.Component");
sap.ui.core.UIComponent.extend("FeatureToggles.Component", {
    metadata: {
        // See https://openui5.hana.ondemand.com/#docs/guide/0187ea5e2eff4166b0453b9dcc8fc64f.html for more details
        version: "1.0",
        includes: [],  // css, javascript files that should be used in the component
        rootView: "FeatureToggles.view.Root",
    },        
});