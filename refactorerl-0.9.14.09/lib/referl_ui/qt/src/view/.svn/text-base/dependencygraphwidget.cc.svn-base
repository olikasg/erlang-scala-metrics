// -*- coding: latin-1 -*-

// This file is part of RefErl.
//
// RefErl is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published
// by the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// RefErl is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with RefErl.  If not, see <http://plc.inf.elte.hu/erlang/>.
//
// The Original Code is RefErl.
//
// The Initial Developer of the Original Code is Eötvös Loránd University.
// Portions created  by Eötvös Loránd University and ELTE-Soft Ltd.
// are Copyright 2007-2013 Eötvös Loránd University, ELTE-Soft Ltd.
// and Ericsson Hungary. All Rights Reserved.

//Author: Mátyás Kuti

#include "dependencygraphwidget.h"
#include "ui_dependencygraphwidget.h"
#include <QFileDialog>
#include <QMessageBox>
#include <QGraphicsSvgItem>
#include <QGraphicsScene>
#include "model/dependencygraph/dotparser.h"
#include "dependencygraph/graphwidget.h"

DependencyGraphWidget::DependencyGraphWidget(QWidget *parent,
                                             RefErlModel *ref_erl_model) :
    QWidget(parent),
    ui(new Ui::DependencyGraphWidget),
    ref_erl_model_(ref_erl_model)
{
    ui->setupUi(this);

    dialog_ = new QProgressDialog(this);
    dialog_->setMaximum(0);
    dialog_->setWindowTitle("Generating SVG file");

    connect( ui->drawSVGButton, SIGNAL( clicked() ),
        this, SLOT( DrawSVGButtonClicked() ) );
    connect( ref_erl_model_, SIGNAL( SVGSignal(QString) ),
        this, SLOT( ShowSVG(QString) ) );
    connect( ref_erl_model_, SIGNAL( GraphSignal(QString) ),
        this, SLOT( ShowGraph(QString) ) );
    connect( ui->addExcludedButton, SIGNAL( clicked() ),
        this, SLOT( AddExcludedButtonClicked() ) );
    connect( ui->addLeavesButton, SIGNAL( clicked() ),
        this, SLOT( AddLeavesButtonClicked() ) );
    connect( ui->deleteExcludedButton, SIGNAL( clicked() ),
        this, SLOT( DeleteExcludedButtonClicked() ) );
    connect( ui->deleteLeavesButton, SIGNAL( clicked() ),
        this, SLOT( DeleteLeavesButtonClicked() ) );
    connect( ref_erl_model_, SIGNAL( ErrorMessageSignal(QString) ),
        this, SLOT( ErrorHandler(QString) ) );
    connect( ui->drawButton, SIGNAL( clicked() ),
        this, SLOT( DrawButtonClicked() ) );

    graph_widget_ = new GraphWidget(this);
    graph_widget_->setScene( new QGraphicsScene(graph_widget_) );
    graph_widget_->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    ui->horizontalLayout->addWidget(graph_widget_);

    ui->levelComboBox->addItems( QStringList { "Module", "Module group", "Function" } );
    ui->typeComboBox->addItems( QStringList { "All", "Cycles" } );

    nodes_completer_ = new QCompleter(this);
    nodes_completer_->setModel(ref_erl_model_->ModuleModel());
    leaves_completer_ = new QCompleter(this);
    leaves_completer_->setModel(ref_erl_model_->ModuleModel());
    start_node_completer_ = new QCompleter(this);
    start_node_completer_->setModel(ref_erl_model_->ModuleModel());

    ui->nodesEdit->setCompleter(nodes_completer_);
    ui->leavesEdit->setCompleter(leaves_completer_);
    ui->startNodeEdit->setCompleter(start_node_completer_);

    excluded_nodes_model_ = new QStringListModel(this);
    excluded_leaves_model_ = new QStringListModel(this);

    ui->excludedListView->setModel(excluded_nodes_model_);
    ui->excludedLeavesListView->setModel(excluded_leaves_model_);

    ref_erl_model_->GetModules();
    ref_erl_model_->GetFunctions();
    connect( ui->levelComboBox, SIGNAL( currentIndexChanged(QString) ),
        this, SLOT( LevelComboboxCurrentTextChanged(QString) ) );
}

DependencyGraphWidget::~DependencyGraphWidget()
{
    delete ui;
}

void DependencyGraphWidget::LevelComboboxCurrentTextChanged(const QString &current)
{
    if(current == "Module") {
        ui->excludedNodesGroupBox->setTitle("Excluded modules");
        nodes_completer_->setModel(ref_erl_model_->ModuleModel());
        leaves_completer_->setModel(ref_erl_model_->ModuleModel());
        start_node_completer_->setModel(ref_erl_model_->ModuleModel());
        excluded_nodes_model_->setStringList(QStringList());
        excluded_leaves_model_->setStringList(QStringList());
        ui->excludedLeavesGroupBox->show();
        ui->excludedNodesGroupBox->show();
        ui->startNodeLabel->show();
        ui->startNodeEdit->show();
        ui->excludeOtpCheckBox->show();
    } else if(current == "Function") {
        ui->excludedNodesGroupBox->setTitle("Excluded functions");
        nodes_completer_->setModel(ref_erl_model_->FunctionModel());
        leaves_completer_->setModel(ref_erl_model_->FunctionModel());
        start_node_completer_->setModel(ref_erl_model_->FunctionModel());
        excluded_nodes_model_->setStringList(QStringList());
        excluded_leaves_model_->setStringList(QStringList());
        ui->excludedLeavesGroupBox->show();
        ui->excludedNodesGroupBox->show();
        ui->startNodeLabel->show();
        ui->startNodeEdit->show();
        ui->excludeOtpCheckBox->show();
    } else {
        ui->excludedLeavesGroupBox->hide();
        ui->excludedNodesGroupBox->hide();
        ui->startNodeLabel->hide();
        ui->startNodeEdit->hide();
        ui->excludeOtpCheckBox->hide();
    }
    ui->startNodeEdit->clear();
    ui->nodesEdit->clear();
    ui->leavesEdit->clear();
}

void DependencyGraphWidget::DrawButtonClicked()
{
    dialog_->setLabelText("Generating .dot file");
    DrawGraph(QString());
}

void DependencyGraphWidget::DrawSVGButtonClicked()
{
    QString path;
    path = QFileDialog::getSaveFileName(this,
                                        "Choose the SVG path",
                                        QString(),
                                        "SVG file (*.svg)" );
    if( path.isEmpty() ) {
        return;
    }

    dialog_->setLabelText(QString("Generating %1").arg(path));

    DrawGraph(path);

}

void DependencyGraphWidget::ShowGraph(const QString &path)
{
    dialog_->accept();
    DotParser d(path);
    DependencyGraph *g = d.Parse();
    graph_widget_->DrawGraph(g);
    delete g;
}

void DependencyGraphWidget::DrawGraph(const QString &path)
{
    DependencyLevel level = Module;
    DependencyType type = All;

    QString level_text = ui->levelComboBox->currentText();
    QString start_node = ui->startNodeEdit->text();
    if( level_text == "Module group" ) {
        level = ModuleGroup;
        start_node = "";
    } else if( level_text == "Module" ) {
        level = Module;
    } else {
        level = Function;
    }

    QString type_text = ui->typeComboBox->currentText();

    if( type_text == "All" ) {
        type = All;
    } else {
        type = Cycle;
    }

    ref_erl_model_->DrawGraph(
        path, level, type,
        ui->excludeOtpCheckBox->isChecked(),
        start_node,
        excluded_nodes_model_->stringList(),
        excluded_leaves_model_->stringList(), !path.isEmpty()
    );

    dialog_->exec();
}

void DependencyGraphWidget::AddExcludedButtonClicked()
{
    QString current = ui->nodesEdit->text();
    if(current == "") return;
    QStringList current_list = excluded_nodes_model_->stringList();
    if(current_list.contains(current)) return;
    current_list << current;
    excluded_nodes_model_->setStringList(current_list);
}

void DependencyGraphWidget::AddLeavesButtonClicked()
{
    QString current = ui->leavesEdit->text();
    if(current == "") return;
    QStringList current_list = excluded_leaves_model_->stringList();
    if(current_list.contains(current)) return;
    current_list << current;
    excluded_leaves_model_->setStringList(current_list);

}

void DependencyGraphWidget::DeleteExcludedButtonClicked()
{
    QModelIndexList selected_list =
            ui->excludedListView->selectionModel()->selectedRows();
    for( int i = 0; i < selected_list.count(); ++i) {
        int row = selected_list.at(i).row();
        excluded_nodes_model_->removeRow(row);
    }
}

void DependencyGraphWidget::DeleteLeavesButtonClicked()
{
    QModelIndexList selected_list =
            ui->excludedLeavesListView->selectionModel()->selectedRows();
    for( int i = 0; i < selected_list.count(); ++i) {
        int row = selected_list.at(i).row();
        excluded_leaves_model_->removeRow(row);
    }
}

void DependencyGraphWidget::ShowSVG(const QString &path)
{
    dialog_->accept();
    QMessageBox::information(
        this, "SVG saved", QString("SVG was saved to %1").arg(path)
    );

    QGraphicsScene *sc = new QGraphicsScene(graph_widget_);
    graph_widget_->setScene(sc);
    sc->clear();
    QGraphicsSvgItem *svg_item = new QGraphicsSvgItem(path);
    svg_item->setZValue(0);
    sc->addItem(svg_item);
}

void DependencyGraphWidget::ErrorHandler(QString)
{
    dialog_->close();
}
