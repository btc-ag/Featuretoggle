sap.ui.controller("FeatureToggles.view.Root", {
    _oDialog: null,

    _oDate: new sap.ui.model.type.Date( { style: 'medium', source: { pattern: 'yyyy-MM-ddThh:mm:ssZ' } }),

    _oFilterText: [],
    _oFilterVariables: [],

    /**
    * Called when a controller is instantiated and its View controls (if available) are already created.
    * Can be used to modify the View before it is displayed, to bind event handlers and do other one-time initialization.
    * @memberOf view.BonitaetsView
    */
    onInit: function () {
        var model = new sap.ui.model.json.JSONModel();
        model.forceNoCache(true);
        this.getView().setModel(model);
        this.reloadData();

        this.mGroupFunctions = {
            "CLASS": function (oContext) {
                var name = oContext.getProperty("CLASS");
                return {
                    key: name,
                    text: name
                };
            },
            "DATE": function (oContext) {
                var name = oContext.getProperty("DATE");
                return {
                    key: name,
                    text: name
                };
            },
            "CURRENTSTATE": function (oContext) {
            var name = oContext.getProperty("CURRENTSTATE");
            return {
                key: name,
                text: name
            };
        }
    }

    },

    /**
    * Similar to onAfterRendering, but this hook is invoked before the controller's View is re-rendered
    * (NOT before the first rendering! onInit() is used for that one!).
    * @memberOf view.BonitaetsView
    */
    //	onBeforeRendering: function() {
    //
    //	},

    /**
    * Called when the View has been rendered (so its HTML is part of the document). Post-rendering manipulations of the HTML could be done here.
    * This hook is the same one that SAPUI5 controls get after being rendered.
    * @memberOf view.BonitaetsView
    */
    //	onAfterRendering: function() {
    //
    //	},

    /**
    * Called when the Controller is destroyed. Use this one to free resources and finalize activities.
    * @memberOf view.BonitaetsView
    */
    //	onExit: function() {
    //
    //	}
    reloadData: function () {
        var model = this.getView().getModel();
        $.ajax("/sap/bc/sy_feature/featurelist.json", { dataType: "json", cache: false })
            .done(function (data) {
                data.DATA.forEach(function (item) { item.NewState = item.CURRENTSTATE; });
                model.setData(data);
            });
    },
    featureActiveSwitchChanged: function (oEvent) {
        var active = oEvent.getParameter("state");
        var context = oEvent.getSource().getBindingContext();
        var that = this;
        if (active) {
            var dependencies = context.getProperty("DEPENDENCIES");
            if (dependencies && dependencies.length > 0) {
                dependencies.forEach(function (item) { that.setFeatureState(item, active); });
            }
        }
        else {
            var feature = context.getProperty("");
            var dependentFeatures = this.getDependentFeatures(feature);
            dependentFeatures.forEach(function (item) { that.setFeatureState(item, active); });
        }
    },
    featureActivationDateChanged: function (oEvent) {
        var date = oEvent.getParameter("dateValue");
        var context = oEvent.getSource().getBindingContext();
        var feature = context.getProperty("");
        var that = this;
        var dependencies = context.getProperty("DEPENDENCIES");
        if (dependencies && dependencies.length > 0) {
            dependencies.forEach(function (item) { that.setFeatureActivationDate(item, date, true); });
        }
        var dependentFeatures = this.getDependentFeatures(feature);
        dependentFeatures.forEach(function (item) { that.setFeatureActivationDate(item, date, false); });
    },
    getDependentFeatures: function (feature) {
        var model = this.getView().getModel();
        var featureList = model.getProperty("/DATA");
        var dependentFeatures = featureList.filter(function (item) { return item.DEPENDENCIES.some(function (dep) { return dep.CLASS === feature.CLASS && dep.NAME === feature.NAME; }); });
        return dependentFeatures;
    },
    getChangedFeatures: function () {
        var model = this.getView().getModel();
        var featureList = model.getProperty("/DATA");
        var changedFeatures = featureList.filter(function (item) { return item.CURRENTSTATE !== item.NewState || item.CURRENTACTIVATIONTIME !== item.NewActivationTime; });
        return changedFeatures;
    },
    savePressed: function (oEvent) {
        var changedFeatures = this.getChangedFeatures();
        var that = this;
        $.sap.require("sap.m.MessageBox");
        sap.m.MessageBox.show($.sap.formatMessage("{0} Features werden geändert", [changedFeatures.length]),
           {
               icon: sap.m.MessageBox.Icon.INFORMATION,
               title: "Features aktivenen/deaktivieren",
               actions: [sap.m.MessageBox.Action.YES, sap.m.MessageBox.Action.NO],
               defaultAction: sap.m.MessageBox.Action.NO,
               initialFocus: sap.m.MessageBox.Action.NO,
               onClose: function (action) {
                   if (action === sap.m.MessageBox.Action.YES) {
                       that.writeChanges();
                   }
               }
           });
    },
    resetPressed: function (oEvent) {
        this.reloadData();
    },
    setFeatureState: function (feature, active) {
        var model = this.getView().getModel();
        var featureList = model.getProperty("/DATA");
        featureList.forEach(function (item, index) {
            if (item.CLASS === feature.CLASS && item.NAME === feature.NAME) {
                model.setProperty($.sap.formatMessage("/DATA/{0}/NewState", [index]), active);
            }
        });
    },
    setFeatureActivationDate: function (feature, date, forwardOnly) {
        var model = this.getView().getModel();
        var featureList = model.getProperty("/DATA");
        featureList.forEach(function (item, index) {
            if (item.CLASS = feature.CLASS && item.NAME === feature.NAME) {
                if (!item.NewActivationTime ||
                    (forwardOnly && item.NewActivationTime > date) ||
                    (!forwardOnly && item.NewActivationTime < date)) {
                    model.setProperty($.sap.formatMessage("/DATA/{0}/NewActivationTime", [index]), date);
                }
            }
        });
    },
    writeChanges: function () {
        var changedFeatures = this.getChangedFeatures();
        var data = {
            DATA: changedFeatures.map(function (item) { return { CLASS: item.CLASS, NAME: item.NAME, ACTIVE: item.NewState }; }, this)
        };
        $.ajax("/sap/bc/sy_feature/featurelist.json", { method: "PUT", data: JSON.stringify(data), context: this }).done(this.writeFeatureDone).fail(this.writeFeatureFailed);
    },
    writeFeatureDone: function (data, textStatus, jqXHR) {
        this.reloadData();
        $.sap.require("sap.m.MessageToast");
        sap.m.MessageToast.show("Änderungen gespeichert. Es kann einige Minuten dauern, bis die Änderungen auf allen Servern aktiv werden.");
    },
    writeFeatureFailed: function (jqXHR, textStatus, errorThrown) {
        $.sap.require("sap.m.MessageBox");
        var bCompact = !!this.getView().$().closest(".sapUiSizeCompact").length;
        sap.m.MessageBox.show(
            "Fehler beim Speichern der Änderungen: " + errorThrown,
            {  title: "Fehler",    styleClass: bCompact ? "sapUiSizeCompact" : ""  }
        );       
        this.reloadData();
    },
    _onDependencyPress: function (oEvent) {
        var button = oEvent.getSource();
        var bindingContext = button.getBindingContext();
        var dependency = bindingContext.getProperty("");
        var table = this.byId("table");
        var items = table.getItems(true);
        var itemsToSelect = items.filter(function (item) { var itemContext = item.getBindingContext(); return itemContext && itemContext.getProperty("CLASS") === dependency.CLASS && itemContext.getProperty("NAME") === dependency.NAME; });
        itemsToSelect.forEach(function (item) { item.setSelected(true); });

    },
    clearActivationTime: function (oEvent) {
        var button = oEvent.getSource();
        var bindingContext = button.getBindingContext();
        bindingContext.getModel().setProperty(bindingContext.getPath() + "/NewActivationTime", undefined);
    },
    _onSetFilter: function() {
        var aFilter = [];
        var binding = this.byId("table").getBinding("items");
        if ((this._oFilterText != null) && (this._oFilterVariables != null))
        {
            aFilter = this._oFilterVariables.concat(this._oFilterText);
        }
        else if  (this._oFilterText != null)
        {
            aFilter = this._oFilterText;
        }
        else if (this._oFilterVariables != null)
        {
            aFilter = this._oFilterVariables;
        }
        if (aFilter.length > 1)
        {
            var filter = new sap.ui.model.Filter({ filters: aFilter, and: true });
            binding.filter(filter, sap.ui.model.FilterType.Application);
        } else {
            binding.filter(aFilter, sap.ui.model.FilterType.Application);
        }
    },
    _onFilterInputChange: function (oEvent) {
        var filterText = oEvent.getParameter("newValue");
        if (this._filterText === filterText) return;
        this._filterText = filterText;
        this._oFilterText = null;       
        if (filterText && filterText.length > 0) {
            var filters = [
                new sap.ui.model.Filter("CLASS", sap.ui.model.FilterOperator.Contains, filterText),
                new sap.ui.model.Filter("NAME", sap.ui.model.FilterOperator.Contains, filterText),
                new sap.ui.model.Filter("DATE", sap.ui.model.FilterOperator.Contains, filterText),
                new sap.ui.model.Filter("DESCRIPTION", sap.ui.model.FilterOperator.Contains, filterText)
            ];
            this._oFilterText = new sap.ui.model.Filter({ filters: filters, and: false });
        }
        this._onSetFilter();
    },
    _getGroupHeader: function (oGroup) {
        return new sap.m.GroupHeaderListItem({ title: oGroup.key });
    },
    handleMenuItemPress: function (oEvent){
        if (oEvent.getParameter("item").getSubmenu()) {
            return;
        }

        if (oEvent.getParameter("item").getId() == "MenuAlleAus")
        {
            var model = this.getView().getModel();
            var featureList = model.getProperty("/DATA");
            featureList.forEach(function (item, index) {               
                    model.setProperty($.sap.formatMessage("/DATA/{0}/NewState", [index]), false);                
            });
        }
    },
    handleZustandButtonPressed: function (oEvent) {
        var oButton = oEvent.getSource();

        // create menu only once
        if (!this._menu) {
            this._menu = sap.ui.xmlfragment(
                "FeatureToggles.fragment.TableMenu",
                this
            );
            this.getView().addDependent(this._menu);
        }

        var eDock = sap.ui.core.Popup.Dock;
        this._menu.open(this._bKeyboard, oButton, eDock.BeginTop, eDock.BeginBottom, oButton);
    },
    handleClearAllFilters: function (oEvent) {
        this.getView().byId("SearchField").setValue("");; 
        this._oFilterText = null;
        this._oFilterVariables = null;
        this._onSetFilter();
    },
    handleViewSettingsDialogButtonPressed: function (oEvent) {
        if (!this._oDialog) {
            this._oDialog = sap.ui.xmlfragment("FeatureToggles.fragment.ViewSettings", this);
        }
        this._oDialog.setModel(this.getView().getModel());
        // toggle compact style
        jQuery.sap.syncStyleClass("sapUiSizeCompact", this.getView(), this._oDialog);
        this._oDialog.open();
    },
    handleViewSettingsConfirm: function (oEvent) {

        var oView = this.getView();
        var oTable = oView.byId("table");

        var mParams = oEvent.getParameters();
        var oBinding = oTable.getBinding("items");

        // apply sorter to binding
        // (grouping comes before sorting)
        var aSorters = [];
        if (mParams.groupItem) {
            var sPath = mParams.groupItem.getKey().split("_");
            var bDescending = mParams.groupDescending;
            var vGroup = this.mGroupFunctions[sPath[1]];
            aSorters.push(new sap.ui.model.Sorter(sPath[1], bDescending, vGroup));
        }
        var sPath = mParams.sortItem.getKey().split("_");
        var bDescending = mParams.sortDescending;
        aSorters.push(new sap.ui.model.Sorter(sPath[1], bDescending));
        oBinding.sort(aSorters);

        // apply filters to binding
        
        this._oFilterVariables = [];

        var filterString = "";
        var oDatePickerFrom = this._oDialog.getFilterItems()[0].getCustomControl();
        if (oDatePickerFrom._bValid) {
            if (oDatePickerFrom._lastValue.length > 0) {
                var oFilterFrom = new sap.ui.model.Filter({ path: "DATE", operator: sap.ui.model.FilterOperator.GE, value1: oDatePickerFrom._lastValue, value2: "X", and: true });
                this._oFilterVariables.push(oFilterFrom);
                var sDate = this._oDate.formatValue(oDatePickerFrom._lastValue, "string");
                filterString = filterString + " Von Datum " + sDate;
            }
        }
        var oDatePickerTo = this._oDialog.getFilterItems()[1].getCustomControl();
        if (oDatePickerTo._bValid) {
            if (oDatePickerTo._lastValue.length > 0) {
                var oFilterTo = new sap.ui.model.Filter({ path: "DATE", operator: sap.ui.model.FilterOperator.LE, value1: oDatePickerTo._lastValue, value2: "X", and: true });
                this._oFilterVariables.push(oFilterTo);
               
                var sDate = this._oDate.formatValue(oDatePickerTo._lastValue, "string");
                filterString = filterString + " Bis Datum " + sDate;
            }
        }

        this._onSetFilter();
  
        // update filter bar
        oView.byId("vsdFilterBar").setVisible(this._oFilterVariables.length > 0);
        oView.byId("vsdFilterLabel").setText(filterString);
    }

});